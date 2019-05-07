from random import choice
import reusablenet as rnet
import tensorflow as tf
import numpy as np


class SegmentationParameters:
    def __init__(
        self,
        n_classes,
        input_dimension,
        latent_dimension,
        encoder_layers,
        training_examples,
        validation_examples,
        decoder_layers=None,
        latent_activation="tanh",
        reconstruction_activation="sigmoid",
        classifier_layers=None,
        reconstructed_likelihood_transform=lambda x: x,
        epsilon=1e-4,
        session=None,
        steps_per_epoch=8192,
        batch_size=32,
    ):
        """Data class for holding parameters associated with segmentation."""
        self.n_classes = n_classes

        self.input_dimension = input_dimension
        self.latent_dimension = latent_dimension

        self.latent_activation = latent_activation
        self.reconstruction_activation = reconstruction_activation

        self.encoder_layers = encoder_layers + [
            (self.latent_dimension, self.latent_activation)
        ]
        self.decoder_layers = (
            decoder_layers if decoder_layers is not None else encoder_layers[::-1]
        ) + [(self.input_dimension, self.reconstruction_activation)]

        self.use_classifier = classifier_layers is not None
        self.classifier_layers = (
            classifier_layers + [(self.n_classes, "softmax")]
            if self.use_classifier
            else None
        )

        self.reconstructed_likelihood_transform = reconstructed_likelihood_transform

        self.epsilon = epsilon

        self.session = session if session is not None else tf.Session()

        self.training_examples = training_examples
        self.validation_examples = validation_examples
        self.steps_per_epoch = steps_per_epoch
        self.batch_size = batch_size

    def default_input_node(self):
        """Create a default placeholder node for the inputs."""
        return rnet.make_input_node([None, self.input_dimension])

    def training_batch(self, size=None):
        """Retrieve a batch of training examples."""
        return [choice(self.training_examples) for _ in range(size or self.batch_size)]

    def validation_batch(self, size=None):
        """Retrieve a batch of validation examples."""
        return [
            choice(self.validation_examples) for _ in range(size or self.batch_size)
        ]


def create_encoders(parameters, input_node):
    """Create an encoder for each class from an input node."""
    return [
        rnet.feedforward_network(
            "class_{}_encoder".format(i),
            parameters.input_dimension,
            parameters.encoder_layers,
            input_node=input_node,
        )
        for i in range(parameters.n_classes)
    ]


def create_decoders(parameters, encoders):
    """Create a decoder for each encoder."""
    return [
        rnet.feedforward_network(
            "class_{}_decoder".format(i),
            parameters.latent_dimension,
            parameters.decoder_layers,
            input_node=encoder["output"],
        )
        for encoder, i in zip(encoders, range(len(encoders)))
    ]


def create_classifier(parameters, input_node):
    """Create a classification network for the classes."""
    if parameters.classifier_layers is None:
        raise Exception("classifier layers unspecified in parameters")
    return rnet.feedforward_network(
        "classifier",
        parameters.input_dimension,
        parameters.classifier_layers,
        input_node=input_node,
    )


def create_composite_reconstruction(parameters, decoders, individual_likelihoods):
    """Multiply each reconstructed input by its likelihood and sum the results."""
    return tf.reduce_sum(
        [
            tf.multiply(
                tf.tile(tf.expand_dims(likelihood, 1), [1, parameters.input_dimension]),
                decoder["output"],
            )
            for decoder, likelihood in zip(decoders, individual_likelihoods)
        ],
        axis=0,
    )


def create_composite_reconstruction_loss(input_node, composite_reconstruction):
    """Create a node that records an overall reconstruction loss."""
    return tf.losses.mean_squared_error(input_node, composite_reconstruction)


def create_individual_reconstruction_loss(input_node, decoder):
    """Create a node that records the reconstruction loss of a single decoder."""
    return 0.5 * tf.reduce_mean(tf.square(input_node - decoder["output"]), axis=1)


def create_individual_reconstruction_losses(input_node, decoders):
    """Create nodes for the reconstruction loss of a number of decoders."""
    return [
        create_individual_reconstruction_loss(input_node, decoder)
        for decoder in decoders
    ]


def create_reconstructed_likelihoods(parameters, individual_reconstruction_losses):
    """Map individual reconstruction losses to [1, 0]."""
    return tf.nn.softmax(
        tf.transpose(
            tf.stack(
                [
                    tf.exp(
                        -parameters.reconstructed_likelihood_transform(
                            reconstruction_loss
                        )
                    )
                    for reconstruction_loss in individual_reconstruction_losses
                ]
            )
        )
    )


def explode_likelihoods(parameters, likelihoods):
    """Explode likelihoods into individual scalar tensors."""
    transposed_likelihoods = tf.transpose(likelihoods)
    return [transposed_likelihoods[i] for i in range(parameters.n_classes)]


def probability_mass_entropy(parameters, probability_mass_distribution):
    """Calculate the entropy of a p.m.f."""
    return tf.reduce_mean(
        tf.reduce_sum(
            -tf.multiply(
                probability_mass_distribution,
                tf.log(probability_mass_distribution) + parameters.epsilon,
            ),
            axis=1,
        )
    )


class DiscreteSegmenter:
    def __init__(self, parameters, input_node=None):
        """Create an object to perform discrete class segmentation on data."""
        self.parameters = parameters
        self.placeholder = (
            input_node
            if input_node is not None
            else self.parameters.default_input_node()
        )

        self.encoders = create_encoders(self.parameters, self.placeholder)
        self.decoders = create_decoders(self.parameters, self.encoders)

        self.individual_likelihoods = (
            create_classifier(self.parameters, self.placeholder)["output"]
            if self.parameters.use_classifier
            else create_reconstructed_likelihoods(
                self.parameters,
                create_individual_reconstruction_losses(
                    self.placeholder, self.decoders
                ),
            )
        )
        self.exploded_likelihoods = explode_likelihoods(
            self.parameters, self.individual_likelihoods
        )

        self.composite_reconstruction = create_composite_reconstruction(
            self.parameters, self.decoders, self.exploded_likelihoods
        )
        self.composite_reconstruction_loss = create_composite_reconstruction_loss(
            self.placeholder, self.composite_reconstruction
        )

        self.entropy_loss = probability_mass_entropy(
            self.parameters, self.individual_likelihoods
        )

        self.entropy_weight = rnet.make_input_node([])
        self.loss = tf.reduce_mean(
            self.composite_reconstruction_loss + self.entropy_weight * self.entropy_loss
        )
        self.optimiser = tf.train.AdamOptimizer().minimize(self.loss)

        self.classification = tf.argmax(self.individual_likelihoods, axis=1)
        self.profile_node = tf.reduce_mean(
            tf.one_hot(self.classification, self.parameters.n_classes), axis=0
        )

    def initialise(self):
        """Initialise training."""
        self.parameters.session.run(tf.global_variables_initializer())

    def perform_epoch(self, entropy_weight, evaluate=True):
        """Perform a single epoch of training."""
        examples_seen = 0
        while examples_seen < self.parameters.steps_per_epoch:
            self.parameters.session.run(
                self.optimiser,
                feed_dict={
                    self.placeholder: self.parameters.training_batch(),
                    self.entropy_weight: entropy_weight,
                },
            )
            examples_seen += self.parameters.steps_per_epoch
        if not evaluate:
            return {}
        evaluation = self.parameters.session.run(
            [self.composite_reconstruction_loss, self.entropy_loss, self.loss],
            feed_dict={
                self.placeholder: self.parameters.training_batch(),
                self.entropy_weight: entropy_weight,
            },
        )
        return {
            "reconstruction_loss": evaluation[0],
            "entropy_loss": evaluation[1],
            "loss": evaluation[2],
            "entropy_weight": entropy_weight,
        }

    def train(
        self,
        epochs,
        entropy_weight_function=lambda w: w ** 4,
        evaluation_frequency=None,
    ):
        """Train the segmenter for a certain number of epochs."""
        for epoch in range(epochs):
            evaluate = (
                evaluation_frequency is not None and epoch % evaluation_frequency == 0
            )
            evaluation = self.perform_epoch(
                entropy_weight_function(epoch / epochs), evaluate=evaluate
            )
            if evaluate:
                evaluation["epoch"] = epoch
                print(evaluation)

    def classify(self, examples):
        """Determine the class that best describes each example."""
        return self.parameters.session.run(
            self.classification, feed_dict={self.placeholder: examples}
        )

    def profile(self, examples):
        """Return, for each class, the proportion of data fitting into that class."""
        return self.parameters.session.run(
            self.profile_node, feed_dict={self.placeholder: examples}
        )

    def get_classification_probabilities(self, examples):
        """Return the probability that each example is in each class."""
        return self.parameters.session.run(
            self.individual_likelihoods, feed_dict={self.placeholder: examples}
        )


def get_mnist_data(scale):
    """Get training and test MNIST data, downsampling images."""
    mnist = tf.keras.datasets.mnist
    (x_train, _), (x_test, _) = mnist.load_data()
    x_train, x_test = x_train / 255.0, x_test / 255.0

    def downsample(data):
        return data[:, ::scale, ::scale]

    def flatten(data):
        return np.reshape(data, [data.shape[0], data.shape[1] * data.shape[2]])

    def process(data):
        return flatten(downsample(data))

    return process(x_train), process(x_test)

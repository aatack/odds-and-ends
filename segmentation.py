import reusablenet as rnet
import tensorflow as tf


class SegmentationParameters:
    def __init__(
        self,
        n_classes,
        input_dimension,
        latent_dimension,
        encoder_layers,
        decoder_layers=None,
        latent_activation="tanh",
        reconstruction_activation="sigmoid",
        classifier_layers=None,
        reconstructed_likelihood_transform=lambda x: x,
    ):
        """Data class for holding parameters associated with segmentation."""
        self.epsilon = 1e-4

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

        self.classifier_layers = (
            classifier_layers + [(self.n_classes, "softmax")]
            if classifier_layers is not None
            else None
        )

        self.reconstructed_likelihood_transform = reconstructed_likelihood_transform

    def default_input_node(self):
        """Create a default placeholder node for the inputs."""
        return rnet.make_input_node([None, self.input_dimension])


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
    classifier = rnet.feedforward_network(
        "classifier",
        parameters.input_dimension,
        parameters.classifier_layers,
        input_node=input_node,
    )
    transposed_output = tf.transpose(classifier["output"])
    classifier["individual_likelihoods"] = [
        transposed_output[i] for i in range(parameters.n_classes)
    ]
    return classifier


def create_composite_reconstruction(decoders, individual_likelihoods):
    """Multiply each reconstructed input by its likelihood and sum the results."""
    return tf.reduce_sum(
        [
            tf.multiply(decoder["output"], likelihood)
            for decoder, likelihood in zip(decoders, individual_likelihoods)
        ],
        axis=0,
    )


def create_composite_reconstruction_loss(input_node, composite_reconstruction):
    """Create a node that records an overall reconstruction loss."""
    return tf.losses.mean_squared_error(
        tf.tile(input_node, [1, tf.shape(composite_reconstruction)[0]]),
        composite_reconstruction,
    )


def create_individual_reconstruction_loss(input_node, decoder):
    """Create a node that records the reconstruction loss of a single decoder."""
    return 0.5 * tf.reduce_mean(
        tf.square(
            tf.tile(input_node, [1, tf.shape(decoder["output"])[0]]) - decoder["output"]
        ),
        axis=1,
    )


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


def probability_mass_entropy(parameters, probability_mass_distribution):
    """Calculate the entropy of a p.m.f."""
    return tf.reduce_sum(
        -tf.multiply(
            probability_mass_distribution,
            tf.log(probability_mass_distribution) + parameters.epsilon,
        ),
        axis=1,
    )

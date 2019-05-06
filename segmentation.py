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

        self.classifier_layers = (
            classifier_layers + [(self.n_classes, "softmax")]
            if classifier_layers is not None
            else None
        )

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
    classifier["outputs"] = [transposed_output[i] for i in range(parameters.n_classes)]
    return classifier
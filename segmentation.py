import reusablenet as rnet


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

import _curveknob as knobs


BEGINNING_UNTIL_CURRENT_FRAME = "Ctrl+L"
CURRENT_FRAME_UNTIL_OLD_END = "Ctrl+J"


def set_selected_lifetimes(lifetime_type, lifetime_start=None, lifetime_end=None):
    nodes = nuke.selectedNodes()
    if len(nodes) != 1:
        nuke.message(
            "You cannot do this with no nodes selected or more than one node selected"
        )
        return

    node = nodes[0]
    curves = node.knobs()["curves"]
    inapplicable_nodes = 0
    for shape in curves.getSelected():
        inapplicable_nodes += _set_shape_lifetime(
            shape,
            node,
            lifetime_type,
            lifetime_start=lifetime_start,
            lifetime_end=lifetime_end,
        )
    if inapplicable_nodes > 0:
        nuke.message(
            "Could not apply lifetime start to {} nodes".format(inapplicable_nodes)
        )


def _set_shape_lifetime(
    shape, node, lifetime_type, lifetime_start=None, lifetime_end=None
):
    if isinstance(shape, knobs.Layer):
        return sum(
            [
                _set_shape_lifetime(
                    sub_shape,
                    node,
                    lifetime_type,
                    lifetime_start=lifetime_start,
                    lifetime_end=lifetime_end,
                )
                for sub_shape in shape
            ]
        )
    elif isinstance(shape, knobs.Shape):
        previous_end = node["lifetime_end"].getValue()
        print(previous_end)
        node["lifetime_type"].setValue(lifetime_type)
        if lifetime_type == 1:
            node["lifetime_end"].setValue(lifetime_end)
        elif lifetime_type == 4:
            node["lifetime_start"].setValue(lifetime_start)
            node["lifetime_end"].setValue(previous_end)
        else:
            nuke.message("Unrecognised lifetime type for this function")
        return 0
    else:
        return 1


def beginning_until_frame():
    set_selected_lifetimes(1, lifetime_end=nuke.frame())


def frame_until_old_end():
    set_selected_lifetimes(4, lifetime_start=nuke.frame())


def add_to_menu(
    beginning_until_frame_shortcut=BEGINNING_UNTIL_CURRENT_FRAME,
    frame_until_old_end_shortcut=CURRENT_FRAME_UNTIL_OLD_END,
):
    menu = nuke.menu("Nodes")
    menu.addCommand(
        "BUF",
        command="beginning_until_frame()",
        shortcut=beginning_until_frame_shortcut,
    )
    menu.addCommand(
        "FUOE", command="frame_until_old_end()", shortcut=frame_until_old_end_shortcut
    )


add_to_menu()

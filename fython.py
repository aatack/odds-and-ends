def fmap(f, collection):
    """
    Pass each value of a functor through a function.  If a list or dictionary
    is supported, they will be mapped elementwise; otherwise, the collection
    will be passed to the mapping on its own.
    """
    if isinstance(collection, list):
        return [f(x) for x in collection]
    elif isinstance(collection, set):
        return {f(x) for x in collection}
    elif isinstance(collection, dict):
        return {k: f(v) for k, v in collection.items()}
    else:
        return f(collection)


def group(f, collection, exclude_keys=False):
    """
    Group the items of a collection by the value they return when passed
    through a keying function.  If the `exclude_keys` flag is set to True,
    a set will be returned instead of a dictionary.
    """
    output = {}
    iterable = (
        collection
        if (isinstance(collection, list) or isinstance(collection, set))
        else collection.values()
    )
    for item in iterable:
        key = f(item)
        if key in output:
            output[key].add(item)
        else:
            output[key] = {item}
    return set(output.values()) if exclude_keys else output


def partition(predicate, collection):
    """
    Split the collection into two sets, one containing those items for which
    the predicate evaluates to True, and the other for which they are False.
    """
    if isinstance(collection, list):
        positives, negatives = [], []
        for item in collection:
            if predicate(item):
                positives.append(item)
            else:
                negatives.append(item)
    elif isinstance(collection, set):
        positives, negatives = set(), set()
        for item in collection:
            if predicate(item):
                positives.add(item)
            else:
                negatives.add(item)
    elif isinstance(collection, dict):
        positives, negatives = {}, {}
        for k, v in collection.items():
            if predicate(v):
                positives[k] = v
            else:
                negatives[k] = v
    else:
        return collection, None if predicate(collection) else None, collection
    return positives, negatives

from json import loads
import urllib.request


class Item:

    def __init__(self, id, name, item_type, wiki_link, category, recipe):
        """Create a data object representing a Factorio item."""
        self.id = id
        self.name = name
        self.type = item_type
        self.wiki_link = wiki_link
        self.category = category
        self.recipe = recipe
        self.raw = len(self.recipe.ingredients) == 0

    @staticmethod
    def from_json(j):
        """Parse an item from a JSON object."""
        return Item(j['id'], j['name'], j['type'], j['wiki_link'], j['category'],
            Recipe.from_json(j['recipe']))


class Recipe:
    
    def __init__(self, time, quantity, ingredients):
        """Create a data object representing an item's recipe."""
        self.time = time
        self.quantity = quantity
        self.ingredients = ingredients

    @staticmethod
    def from_json(j):
        """Parse a recipe from a JSON object."""
        return Recipe(j['time'], j['yield'],
            {i['id']: i['amount'] for i in j['ingredients']})


def load_items():
    """Load items from the online database."""
    response = urllib.request.urlopen(
        'https://kevinta893.github.io/factorio-recipes-json/recipes.json'
    )
    return loads(response.read().decode())


items = {item.id: item for item in [Item.from_json(x) for x in load_items()]}
items_by_name = {item.name: item for item in items.values()}


def union(a, b):
    """Return a list of the total items in both sets."""
    c = a.copy()
    for item, quantity in b.items():
        if item in c:
            c[item] += quantity
        else:
            c[item] = quantity
    return c


def intersection(a, b):
    """Return the set of items that are in both sets."""
    c = {}
    for item, quantity in a.items():
        if item in b:
            c[item] = min(a[item], b[item])
    return c


def difference(a, b):
    """Return the set of items that are in a but not in b."""
    c = {}
    for item, quantity in a.items():
        if item in b and b[item] < quantity:
            c[item] = quantity - b[item]
        elif item not in b:
            c[item] = quantity
    return c


def multiply(a, k):
    """Multiply all quantities in an item set by a fixed amount."""
    return {item: quantity * k for item, quantity in a.items()}


def raw_ingredients_exact(item_set):
    """Determine the raw ingredients required to make the items in the set."""
    remaining = item_set.copy()
    raw_ingredients = {}
    while any(remaining):
        item, quantity = list(remaining.items())[0]
        item_object = items[item]
        del remaining[item]
        if item_object.raw:
            raw_ingredients = union(raw_ingredients, {item: quantity})
        else:
            item_list = item_object.recipe.ingredients.items()
            for ingredient, ingredient_quantity in item_list:
                remaining = union(remaining, multiply(
                    {ingredient: ingredient_quantity},
                    quantity / item_object.recipe.quantity))
    return raw_ingredients


print(raw_ingredients_exact({'production-science-pack': 1}))

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

    def raw_ingredients(self, leftovers=None, item_lookup=None):
        """
        Determine the raw ingredients needed to produce the item.
        
        To use leftovers, specify the leftovers as an empty dictionary.  To
        exclude leftovers, set it to None instead.
        """
        if self.raw:
            return {self.id: 1}
        else:
            raise NotImplementedError()


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
            [{i['id']: i['amount']} for i in j['ingredients']])


def load_items():
    """Load items from the online database."""
    response = urllib.request.urlopen(
        'https://kevinta893.github.io/factorio-recipes-json/recipes.json'
    )
    return loads(response.read().decode())


items = {item.id: item for item in [Item.from_json(x) for x in load_items()]}
items_by_name = {item.name: item for item in items.values()}

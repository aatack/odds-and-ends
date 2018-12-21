from random import choice


class WordSearch(object):
    ROW_LENGTH = 10000

    def __init__(self, grid):
        """
        Initialise a grid by finding all of its rows and columns,
        and combining them into a single list of strings.
        """
        self.grid = grid
        self._rows = self._get_rows()
        self._columns = self._get_columns()
        self._strings = self._columns + self._rows

    def is_present(self, word):
        """
        Return True iff the word is present in one of the
        vertical columns or horizontal rows of the grid.
        """
        word_length = len(word)
        if word_length > WordSearch.ROW_LENGTH:
            return False
        elif word_length == 0:
            raise Exception('Input word should not be empty.')

        for string in self._strings:
            if word in string:
                return True
        return False

    def _get_rows(self):
        """
        Extract horizontal rows from the grid as a
        list of strings.
        """
        return [self.grid[i * WordSearch.ROW_LENGTH:(i + 1) * \
            WordSearch.ROW_LENGTH] for i in range(WordSearch.ROW_LENGTH)]

    def _get_columns(self):
        """
        Extract vertical columns from the grid as a list
        of strings.
        """
        domain = range(WordSearch.ROW_LENGTH)
        return [''.join([self._rows[column][row] \
            for column in domain]) for row in domain]


characters = 'abcdefghijklmnopqrstuvwxyz'


def random_word(length):
    """
    Generate a random (not necessarily legible) word
    of the given length.
    """
    return ''.join([choice(characters) for _ in range(length)])


def random_grid():
    """
    Generate a random grid of characters, as a single
    string, using WordSearch.ROW_LENGTH to determine the size.
    """
    return random_word(WordSearch.ROW_LENGTH * WordSearch.ROW_LENGTH)

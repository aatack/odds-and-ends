from random import choice


class WordSearch(object):
    ROW_LENGTH = 3

    def __init__(self, grid):
        self.grid = grid
        self.strings = self._get_columns() + self._get_rows()

    def is_present(self, word):
        """
        Return True iff the word is present in one of the
        vertical columns or horizontal rows of the grid.
        """
        for string in self.strings
            if word in string:
                return True
        return False

    def _get_columns(self):
        return [self.grid[i * WordSearch.ROW_LENGTH:(i + 1) * \
            WordSearch.ROW_LENGTH] for i in range(WordSearch.ROW_LENGTH)]

    def _get_rows(self):
        domain = range(WordSearch.ROW_LENGTH)
        return [''.join([self.columns[column][row] \
            for column in domain]) for row in domain]


characters = 'abcdefghijklmnopqrstuvwxyz'


def random_word(length):
    return ''.join([choice(characters) for _ in range(length)])


def random_grid():
    return random_word(WordSearch.ROW_LENGTH * WordSearch.ROW_LENGTH)

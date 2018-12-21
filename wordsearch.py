from random import choice
from math import floor


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
        if self._is_edge_case(word):
            return False

        for string in self._strings:
            if word in string:
                return True
        return False

    def is_present_multicore(self, n_cores, word):
        """
        Return True iff the word is present in one of the
        vertical columns or horizontal rows of the grid, adapted
        such that the process could in theory be executed on
        multiple cores.
        """
        if self._is_edge_case(word):
            return False

        # These jobs would be run in parallel
        jobs = [self._is_present_in_block(n_cores, core, word) \
            for core in range(n_cores)]

        for result in jobs:
            if result:
                return True
        return False

    def _is_edge_case(self, word):
        """
        Returns True if the word is too long to be contained in
        the grid.  If the word's length is 0, throws an exception.
        Otherwise returns False.  It is assumed that the word
        contains only allowed characters, so this is not checked.
        """
        word_length = len(word)
        if word_length > WordSearch.ROW_LENGTH:
            return False
        elif word_length == 0:
            raise Exception('Input word should not be empty.')
        else:
            return True

    def _is_present_in_block(self, n_blocks, block, word):
        """
        Adaptation of is_present for running in multiple threads.
        The rows and columns are split into a certain number of
        blocks, and each block is checked individually to see
        whether it contains the word.  If any of the blocks come
        back as true, the word is contained in the wordsearch.
        """
        block_width = WordSearch.ROW_LENGTH / n_blocks
        # Flooring here instead of earlier prevents OBOEs
        block_start = floor(block_width * block)
        block_end = floor(block_width * (block + 1))

        for string in self._strings[block_start:block_end]:
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

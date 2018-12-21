from random import choice


class WordSearch(object):
    ROW_LENGTH = 100

    def __init__(self, grid):
        self.grid = grid
        self.columns = self._get_columns()
        self.rows = self._get_rows()

    def is_present(self, word):
        """
        Return True iff the word is present in one of the
        vertical columns or horizontal rows of the grid.
        """
        for column in self.columns:
            if word in column:
                return True
        for row in self.rows:
            if word in row:
                return True
        return False

    def _get_columns(self):
        return [self.grid[i * WordSearch.ROW_LENGTH:(i + 1) * WordSearch.ROW_LENGTH] \
            for i in range(WordSearch.ROW_LENGTH)]

    def _get_rows(self):
        domain = range(WordSearch.ROW_LENGTH)
        return [[self.columns[row][column] \
            for column in domain] for row in domain]

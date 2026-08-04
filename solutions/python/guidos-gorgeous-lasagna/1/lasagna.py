"""Functions used in preparing Guido's gorgeous lasagna.

Learn about Guido, the creator of the Python language: https://en.wikipedia.org/wiki/Guido_van_Rossum
"""

# TODO: define the 'EXPECTED_BAKE_TIME' constant
EXPECTED_BAKE_TIME = 40

# TODO: consider defining the 'PREPARATION_TIME' constant
#       equal to the time it takes to prepare a single layer

PREPARATION_TIME = 2


# TODO: define the 'bake_time_remaining()' function
def bake_time_remaining(elapsed_bake_time: int):
    """Calculate the bake time remaining.

    :param elapsed_bake_time: int - baking time already elapsed.
    :return: int - remaining bake time (in minutes) derived from 'EXPECTED_BAKE_TIME'.

    Function that takes the actual minutes the lasagna has been in the oven as
    an argument and returns how many minutes the lasagna still needs to bake
    based on the `EXPECTED_BAKE_TIME`.
    """

    return EXPECTED_BAKE_TIME - elapsed_bake_time


# TODO: define the 'preparation_time_in_minutes()' function
#       and consider using 'PREPARATION_TIME' here
def preparation_time_in_minutes(number_of_layers: int):
    """Calculate the total preparation time for the lasagna layers.

    :param number_of_layers: int - number of layers in the lasagna.
    :return: int - total preparation time in minutes derived from 'PREPARATION_TIME'.

    Function that takes the numbers of layers in the lasagna and returns how many minutes it takes to prepare the layers based on      the `PREPARATION_TIME` per layer.
    """

    return number_of_layers * PREPARATION_TIME


# TODO: define the 'elapsed_time_in_minutes()' function
def elapsed_time_in_minutes(number_of_layers: int, elapsed_bake_time: int):
    """Calculate the total number of minutes you've been cooking.
    
    :param number_of_layers: int - number of layers in the lasagna.
    :param elapsed_bake_time: int - number of minutes lasagna has been baking.
    :return: int - total number of minutes cooking, or sum of prep time and baking time.
    
    Function that takes the numbers of layers in the lasagna, and the amount of time the lasagna has been cooking, and returns the     total elapsed time in minutes.
    """
    return preparation_time_in_minutes(number_of_layers) + elapsed_bake_time

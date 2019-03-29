# Standard Library
from math import cos, e, exp, gcd, pi, pow
from statistics import mode, stdev, variance

# 3rd Party
import numpy as np
import pandas as pd
from numpy import (
    abs, arange, cast, ceil, column_stack, concatenate, cos, dot, empty, floor, fromfunction, fromiter, full, linspace,
    log2, log10, matrix, mean, median, ones, prod, round, row_stack, sin, std, sum, tan, trunc, var, vstack, zeros
)
from numpy.linalg import det
from numpy.polynomial import Polynomial
from numpy.random import multinomial, normal, poisson, rand, randint, randn, triangular, uniform
from pandas import DataFrame as DF, Series as S, read_csv, read_json

np.set_printoptions(suppress=True, precision=3)
np.ALLOW_THREADS = 4

from typing import List

import numpy as np
import pandas as pd


def get_intervals(start: float, end: float, b: float, step=None) -> List[pd.Interval]:
    if step is None:
        step = np.around(b, 1)
    return [pd.Interval(left=round(x, 3), right=round(x + b, 3), closed="both") for x in np.arange(start, end, step)]


def index_range(df, start, stop):
    return [i for i in df.index if i.left >= start and i.right < stop]


def class_bnd(df, n: int, bnd="low"):
    """
    rerurn lower or upper class boundary
    df: pd.DataFrame/list pd.Interval - groupped table
    n: nth class
    bnd: low,up for lower and upper boundary
    """
    if isinstance(df, pd.DataFrame):
        if n == 0 and bnd == "low":
            return df.iloc[0].name.left - 0.5 * np.around(df.iloc[1].name.left - df.iloc[0].name.right, 2)
        elif n == len(df) - 1 and bnd == "up":
            return df.iloc[n].name.right + 0.5 * np.around(df.iloc[n].name.left - df.iloc[n - 1].name.right, 2)
        if bnd == "low":
            return 0.5 * (df.iloc[n].name.left + df.iloc[n - 1].name.right)
        elif bnd == "up":
            return 0.5 * (df.iloc[n].name.right + df.iloc[n + 1].name.left)
        else:
            raise Exception("Unknown class boundary")
    elif isinstance(df, list) and (len(df)) and isinstance(df[0], pd.Interval):
        if n == 0 and bnd == "low":
            return df[0].left - 0.5 * np.around(df[1].left - df[0].right, 2)
        elif n == len(df) - 1 and bnd == "up":
            return df[n].right + 0.5 * np.around(df[n].left - df[n - 1].right, 2)
        elif bnd == "low":
            return 0.5 * (df[n].left + df[n - 1].right)
        elif bnd == "up":
            return 0.5 * (df[n].right + df[n + 1].left)
        else:
            raise Exception("Unknown class boundary")
    else:
        raise Exception("Unknown class boundary")


def quantile_groups(df, n, freq_col, cum_freq=None, d=1e-3):
    """
    :param df: pd.DataFrame
    :param n: number of parts
    :param freq_col: column of frequencies
    :param cum_freq: column of cumulative frequencies
    :param d: float, small addition to round
    :return: list of values
    """
    N = df[freq_col].sum()
    q = N / n
    quantiles = []
    if cum_freq is None:
        cum_freq = "Cum_freq"
        df['Cum_freq'] = df[freq_col].cumsum()
    for i in range(1, n):
        h = i * q
        for k, ((j0, row0), (j1, row1)) in enumerate(zip(df[:-1].iterrows(), df[1:].iterrows())):
            if row0.Cum_freq < h and row1.Cum_freq >= h:
                L1 = class_bnd(df, k, 'up')
                p = h - df.loc[j0][cum_freq]
                g = df.loc[j1][freq_col]
                c = class_bnd(df, k + 1, bnd='up') - class_bnd(df, k + 1, bnd='low')
                quantiles.append(np.around(d + L1 + p * c / g, 2))
                break
            elif row0.Cum_freq > h:
                L1 = class_bnd(df, k, 'low')
                p = h
                g = df.loc[j0][freq_col]
                c = class_bnd(df, k + 1, bnd='up') - class_bnd(df, k + 1, bnd='low')
                quantiles.append(np.around(d + L1 + p * c / g, 2))
                break
    return quantiles


def table23():
    b = 9.99
    start = 250
    end = 320
    df = pd.DataFrame([8, 10, 16, 14, 10, 5, 2], columns=['Number of Emoyees'], index=get_intervals(start, end, b))
    df.index.name = "Wages"
    return df
    
def table21():
    b = 2
    start = 60
    end = 74
    df = pd.DataFrame([5,18,42,27,8], columns=['Frequency'], index=get_intervals(start,end,b,step=3))
    df.index.name = "Height"
    return df


if __name__ == '__main__':
    df = table23()
    freq_col = "Number of Emoyees"
    cum_freq = None
    n = 4  # на сколько частей делить выборку
    Q = quantile_groups(df, n, freq_col)
    print(Q)

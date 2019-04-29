import numpy as np
import pandas as pd
import util.yf as yf


if __name__ == "__main__":
    # time frame
    start_date = '2018-01-01'
    end_date = '2019-01-01'

    # market returns
    market_returns = yf.get_market_returns(start_date, end_date)
    print('Avg. Market Return:', np.mean(market_returns))
    pass
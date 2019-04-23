import random
import numpy as np
import pandas as pd
import fix_yahoo_finance as yf


def get_SP500_tickers():
    data = pd.read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies')
    table = data[0]
    tickers = table['Symbol'].tolist()
    return tickers

def get_ticker_returns(ticker, start_date, end_date):
    data_stock = yf.download(ticker, start=start_date, end=end_date)
    adj_close_stock = data_stock['Adj Close'].values
    returns_stock = 1 - adj_close_stock[:-1] / adj_close_stock[1:]
    print('Successfully retrieved '+ticker+' data from '+start_date+' to '+end_date+': '+str(len(returns_stock))+' trading days')
    return returns_stock

def get_market_returns(start_date, end_date):
    return get_ticker_returns("^GSPC", start_date, end_date)
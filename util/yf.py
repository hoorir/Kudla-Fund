import random
import numpy as np
import pandas as pd
import fix_yahoo_finance as yf
from typing import List


def get_SP500_tickers():
    data = pd.read_html('https://en.wikipedia.org/wiki/List_of_S%26P_500_companies')
    table = data[0]
    tickers = table['Symbol'].tolist()
    return tickers

def get_ticker_returns(ticker: str, start_date: str, end_date: str, method='log') -> List:
    data_stock = yf.download(ticker, start=start_date, end=end_date)
    adj_close_stock = data_stock['Adj Close'].values
    if method == 'arithmetic':
        returns_stock = adj_close_stock[1:] / adj_close_stock[:-1] - 1
    elif method == 'log':
        returns_stock = np.log(adj_close_stock[1:] / adj_close_stock[:-1])
    print('Successfully retrieved '+ticker+' data from '+start_date+' to '+end_date+': '+str(len(returns_stock))+' trading days')
    return returns_stock

def get_risk_free_rate(start_date: str, end_date: str) -> List:
    data_rf = yf.download("^IRX", start=start_date, end=end_date)
    adj_close_rf = data_rf['Adj Close'].values / 100
    returns_rf = (adj_close_rf / 252)[0:]
    print('Successfully retrieved risk-free rate from'+start_date+' to '+end_date+': '+str(len(returns_rf))+' trading days')
    return returns_rf

def get_market_returns(start_date: str, end_date: str) -> List:
    return get_ticker_returns("^GSPC", start_date, end_date)

def get_sample_vCV(returns_mat: np.ndarray) -> np.ndarray:
    returns_means = np.mean(returns_mat, axis=1, keepdims=True)
    return (1/T) * np.matmul(returns_mat-returns_means, (returns_mat-returns_means).T)

def get_target_VCV(returns_mat: np.ndarray) -> np.ndarray:
    N, _ = returns_mat.shape
    sample_VCV = get_sample_vCV(returns_mat)
    sqrt_s = np.sqrt(np.diag(sample_VCV)).reshape((-1, 1))
    R = (sample_VCV / sqrt_s) / sqrt_s.T
    r_bar = (np.sum(R) - N) / (N * (N-1))
    out = r_bar * np.matmul(sqrt_s, sqrt_s.T)
    np.fill_diagonal(out, sqrt_s)
    return out

def get_shrunk_VCV(returns_mat: np.ndarray) -> np.ndarray:
    N, T = returns_mat.shape
    returns_means = np.mean(returns_mat, axis=1, keepdims=True)
    X = (returns_mat-returns_means).T
    sample_VCV = (1/T) * np.matmul(X.T, X)
    var = np.diag(sample_VCV).reshape((-1, 1))
    sqrt_s = np.sqrt(var)
    R = (sample_VCV / sqrt_s) / sqrt_s.T
    r_bar = (np.sum(R) - N) / (N * (N-1))
    target_VCV = r_bar * np.matmul(sqrt_s, sqrt_s.T)
    np.fill_diagonal(target_VCV, sqrt_s)
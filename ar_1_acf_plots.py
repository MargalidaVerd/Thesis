import numpy as np
from statsmodels.tsa.arima.model import ARIMA
import pmdarima
import matplotlib.pyplot as plt
import statsmodels.api as sm
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf


phi_positive = 0.8
phi_negative = -0.8

model_1 = ARIMA(endog=[0], order=(1,0,0))
model_2 = ARIMA(endog=[0], order=(1,0,0))

simulated_data_positive = model_1.simulate(params = [phi_positive,0,0], nsimulations=100)
simulated_data_negative = model_2.simulate(params = [phi_negative,0,0], nsimulations=100)


plot_acf(simulated_data_positive)
plt.show()

plot_acf(simulated_data_negative)
plt.show()

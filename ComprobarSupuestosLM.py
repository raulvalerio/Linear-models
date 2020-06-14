
# coding: utf-8

# In[1]:

import pandas as pd
from sklearn.datasets import load_boston


# In[4]:

#cargar datos
boston = load_boston()
print(boston.DESCR)


# In[102]:

X = pd.DataFrame(boston.data,columns=boston.feature_names)
X.head()


# In[103]:

X.drop('CHAS',axis=1,inplace=True)


# In[104]:

X.head()


# In[12]:

y=pd.Series(boston.target,name='MEDV')


# In[23]:

import statsmodels.api as sm
X_constant= sm.add_constant(X)
lin_reg = sm.OLS(y,X_constant).fit()
lin_reg.summary()


# In[66]:

get_ipython().magic('matplotlib inline')
import seaborn as sns
import matplotlib.pyplot as plt
import statsmodels.stats.api as sms


# In[50]:

resid_val =  lin_reg.resid
fitted_val= lin_reg.predict()
## Expected mean of residuals
#E(resid) = 0
lin_reg.resid.mean()


# In[ ]:

#Normality of residuals  with  Shapiro-Wilk test
from scipy import stats
stats.shapiro(lin_reg.resid)
sm.qqplot(resid_val,line='s')


# In[56]:

#Linearity in model
sns.regplot(x=fitted_val, y=y, lowess=True, line_kws={'color': 'red'})
plt.title('Fitted vrs Observed')


# In[146]:

#Homocedasticity
#sns.regplot(x=fitted_val, y=resid_val, lowess=True, line_kws={'color': 'red'})
#resid_stand =  lin_reg.get_influence().resid_studentized_internal
sns.regplot(x=fitted_val, y=resid_stand, lowess=True, line_kws={'color': 'red'})
plt.title('Fitted vrs Residual')


# In[148]:

fig, ax = plt.subplots(figsize=(12,8))
sm.graphics.influence_plot(lin_reg, alpha  = 0.05,ax=ax, criterion="cooks")


# In[161]:

X.loc[365:372,:]


# In[ ]:

#Breusch-Pagan test
bp_test= sms.het_breuschpagan(resid_val, lin_reg.model.exog)
print(bp_test)
print("Breush-Pagan Test:  pvalue = ",bp_test[1])


# In[ ]:

## Check Durbin -Watson  in summary for autocorrelation, is it 2?   values < 2 indicates Positive autocorrelation
# use GLS
#Correlation between explanatory variables?
from scipy.stats.stats import pearsonr
X.columns
pearsonr(X['TAX'],lin_reg.resid)
#corr and p-value
# p-value <0.05?  then reject Ho. if not, lack of correlation


# In[84]:

sns.heatmap(X.corr(), annot=True)


# In[55]:

#Multicollinearity
from statsmodels.stats.outliers_influence import variance_inflation_factor
vif = [variance_inflation_factor(X_constant.values, i) for i in range(X_constant.shape[1])]
pd.DataFrame({'vif': vif[1:]}, index=X.columns).T
#Remove VIF larger than 5 (sometimes it is permitted until 10)


# In[ ]:




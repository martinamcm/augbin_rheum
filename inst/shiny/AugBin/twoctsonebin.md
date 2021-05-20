---
title: ""
output: html_document
---
<style type="text/css">

body{ /* Normal  */
      font-size: 12px;
  }
  
title {
  font-size: 38px;
  color: DarkRed;
}
</style>



 <font size="4">  **Latent Variable Model** </font>

Let $\mathbf{Y_{i}}=(Y_{i1}, Y_{i2}, Y_{i3})^{T}$ represent the vector of observed outcomes for patient $i \in N$ and $\mathbf{Y}=(\mathbf{Y_{1}},\ldots \mathbf{Y_{N}})^{T}$ represent the observed outcomes for all patients. $Y_{i1}$ and $Y_{i2}$ are the observed continuous measures. Let $Y_{i3}$ denote the observed binary variable for latent $Y_{i3}^{*}$. We therefore let $\mathbf{Y_{i}^{*}}=(Y_{i1}^{}, Y_{i2}^{}, Y_{i3}^{*})^{T}$ denote the vector of observed and latent continuous measures for patient i and $\mathbf{Y^{*}}=(\mathbf{Y_{1}^{*}},\ldots \mathbf{Y_{N}^{*}})^{T}$. $T_{i}$ represents the treatment indicator for patient i, $y_{i10}$ and $y_{i20}$ are the baseline measures for $Y_{i1}$ and $Y_{i2}$ respectively. 

The mean structure for the outcomes is shown below. The baseline measures $y_{10}$ and $y_{20}$ are included in the model for $Y_{1}$ and $Y_{2}$ respectively. 

$$\begin{align}\label{model1}
\begin{split}
Y_{i1}&=\alpha_{0}+\alpha_{1}T_{i}+\alpha_{2}y_{i10}+\varepsilon_{i1}\\
Y_{i2}&=\beta_{0}+\beta_{1}T_{i}+\beta_{2}y_{i20}+\varepsilon_{i2}\\
Y_{i3}^{*}&=\gamma_{0}+\gamma_{1}T_{i}+\varepsilon_{i3}^{*}
\end{split}
\end{align}$$

The observed binary variable is related to its latent continuous variable by partitioning the latent variable space.

$$\begin{align}\label{discrete}
Y_{i3} = \begin{cases}
  0, & \text{if }  Y_{i3}^{*}< 0, \\
  1 , & \text{if } Y_{i3}^{*} \geq 0
  \end{cases}
\end{align}$$

Following these assumptions, we can model the error terms as multivariate normal with zero mean and variance-covariance matrix $\Sigma$. Note that the error variances for $\varepsilon_{3}^{*}$ is $\sigma_{3}=1$. 

$$\begin{align}\label{variance}
(\varepsilon_{i1}^{}, \varepsilon_{i2}^{}, \varepsilon_{i3}^{*}) \sim N(\boldsymbol{0}, \Sigma) &&
\Sigma=\begin{pmatrix}
\sigma_{1}^{2} & \rho_{12}\sigma_{1}\sigma_{2} & \rho_{13}\sigma_{1}  \\
\rho_{12}\sigma_{1}\sigma_{2} & \sigma_{2}^{2} & \rho_{23}\sigma_{2}  \\
\rho_{13}\sigma_{1} &  \rho_{23}\sigma_{2} & 1  \\
\end{pmatrix}
\end{align}$$

The joint likelihood contribution for patient i with for instance, $Y_{i3}=0$, can be factorised as shown below

$$\begin{equation}\label{likecond}
l\left(\boldsymbol{\theta};\mathbf{Y_{i}^{*}}\right)=f(Y_{i1}^{},Y_{i2}^{};\boldsymbol{\theta})\int_{-\infty}^{0}f(Y_{i3}^{*}|Y_{i1}^{},Y_{i2}^{};\boldsymbol{\theta})dy_{3}^{*}
\end{equation}$$




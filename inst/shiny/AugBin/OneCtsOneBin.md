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

Let $\mathbf{Y_{i}}=(Y_{i1}, Y_{i2})^{T}$ represent the vector of observed outcomes for patient $i \in N$ and $\mathbf{Y}=(\mathbf{Y_{1}},\ldots \mathbf{Y_{N}})^{T}$ represent the observed outcomes for all patients. $Y_{i1}$ is the observed continuous measures. Let $Y_{i2}$ denote the observed binary variable for latent $Y_{i2}^{*}$. We therefore let $\mathbf{Y_{i}^{*}}=(Y_{i1}^{}, Y_{i2}^{*})^{T}$ denote the vector of observed and latent continuous measures for patient i and $\mathbf{Y^{*}}=(\mathbf{Y_{1}^{*}},\ldots \mathbf{Y_{N}^{*}})^{T}$. $T_{i}$ represents the treatment indicator for patient i

The observed binary variable is related to its latent continuous variable by partitioning the latent variable space.

$$\begin{align}
Y_{i2} = \begin{cases}
  0, & \text{if }  Y_{i2}^{*}< 0, \\
  1 , & \text{if } Y_{i2}^{*} \geq 0
  \end{cases}
\end{align}$$

Following these assumptions, we can model the error terms as multivariate normal with zero mean and variance-covariance matrix $\Sigma$. Note that the error variances for $\varepsilon_{2}^{*}$ is $\sigma_{2}=1$. 

$$\begin{align}
(\varepsilon_{i1}^{}, \varepsilon_{i2}^{*}) \sim N(\boldsymbol{0}, \Sigma) &&
\Sigma=\begin{pmatrix}
\sigma_{1}^{2} & \rho_{12}\sigma_{1}  \\
\rho_{12}\sigma_{1} & 1   \\
\end{pmatrix}
\end{align}$$




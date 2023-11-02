use std::{cmp::Ordering, fmt, ops::*};

/// Uiua's complex number type
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct Complex {
    /// The real part
    pub re: f64,
    /// The imaginary part
    pub im: f64,
}

impl Complex {
    /// Create a new complex number
    pub fn new(re: f64, im: f64) -> Self {
        Self { re, im }
    }
    /// Get the minimum of the real and imaginary parts of two complex numbers, ignoring NaN
    pub fn min(self, rhs: impl Into<Self>) -> Self {
        let rhs = rhs.into();
        Self {
            re: self.re.min(rhs.re),
            im: self.im.min(rhs.im),
        }
    }
    /// Get the maximum of the real and imaginary parts of two complex numbers, ignoring NaN
    pub fn max(self, rhs: impl Into<Self>) -> Self {
        let rhs = rhs.into();
        Self {
            re: self.re.max(rhs.re),
            im: self.im.max(rhs.im),
        }
    }
    /// Get the floor of the real and imaginary parts of a complex number
    pub fn floor(self) -> Self {
        Self {
            re: self.re.floor(),
            im: self.im.floor(),
        }
    }
    /// Get the ceiling of the real and imaginary parts of a complex number
    pub fn ceil(self) -> Self {
        Self {
            re: self.re.ceil(),
            im: self.im.ceil(),
        }
    }
    /// Round the real and imaginary parts of a complex number
    pub fn round(self) -> Self {
        Self {
            re: self.re.round(),
            im: self.im.round(),
        }
    }
    /// Get the absolute value of a complex number
    pub fn abs(self) -> f64 {
        self.re.hypot(self.im)
    }
    /// Get the arctangent of a complex number
    pub fn atan2(self, rhs: impl Into<Self>) -> f64 {
        self.abs().atan2(rhs.into().abs())
    }
}

impl From<f64> for Complex {
    fn from(re: f64) -> Self {
        Self { re, im: 0.0 }
    }
}

impl fmt::Display for Complex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.im == 0.0 {
            self.re.fmt(f)
        } else {
            write!(f, "{}+{}i", self.re, self.im)
        }
    }
}

impl Add for Complex {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            re: self.re + rhs.re,
            im: self.im + rhs.im,
        }
    }
}

impl Add<f64> for Complex {
    type Output = Self;
    fn add(self, rhs: f64) -> Self::Output {
        Self {
            re: self.re + rhs,
            im: self.im,
        }
    }
}

impl Add<Complex> for f64 {
    type Output = Complex;
    fn add(self, rhs: Complex) -> Self::Output {
        Complex {
            re: self + rhs.re,
            im: rhs.im,
        }
    }
}

impl Sub for Complex {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            re: self.re - rhs.re,
            im: self.im - rhs.im,
        }
    }
}

impl Sub<f64> for Complex {
    type Output = Self;
    fn sub(self, rhs: f64) -> Self::Output {
        Self {
            re: self.re - rhs,
            im: self.im,
        }
    }
}

impl Sub<Complex> for f64 {
    type Output = Complex;
    fn sub(self, rhs: Complex) -> Self::Output {
        Complex {
            re: self - rhs.re,
            im: -rhs.im,
        }
    }
}

impl Mul for Complex {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            re: self.re * rhs.re - self.im * rhs.im,
            im: self.re * rhs.im + self.im * rhs.re,
        }
    }
}

impl Mul<f64> for Complex {
    type Output = Self;
    fn mul(self, rhs: f64) -> Self::Output {
        Self {
            re: self.re * rhs,
            im: self.im * rhs,
        }
    }
}

impl Mul<Complex> for f64 {
    type Output = Complex;
    fn mul(self, rhs: Complex) -> Self::Output {
        Complex {
            re: self * rhs.re,
            im: self * rhs.im,
        }
    }
}

impl Div for Complex {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        let denom = rhs.re * rhs.re + rhs.im * rhs.im;
        Self {
            re: (self.re * rhs.re + self.im * rhs.im) / denom,
            im: (self.im * rhs.re - self.re * rhs.im) / denom,
        }
    }
}

impl Div<f64> for Complex {
    type Output = Self;
    fn div(self, rhs: f64) -> Self::Output {
        Self {
            re: self.re / rhs,
            im: self.im / rhs,
        }
    }
}

impl Div<Complex> for f64 {
    type Output = Complex;
    fn div(self, rhs: Complex) -> Self::Output {
        let denom = rhs.re * rhs.re + rhs.im * rhs.im;
        Complex {
            re: self * rhs.re / denom,
            im: -self * rhs.im / denom,
        }
    }
}

impl Rem for Complex {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self::Output {
        Self {
            re: self.re % rhs.re,
            im: self.im % rhs.im,
        }
    }
}

impl Rem<f64> for Complex {
    type Output = Self;
    fn rem(self, rhs: f64) -> Self::Output {
        Self {
            re: self.re % rhs,
            im: self.im % rhs,
        }
    }
}

impl Rem<Complex> for f64 {
    type Output = Complex;
    fn rem(self, rhs: Complex) -> Self::Output {
        Complex {
            re: self % rhs.re,
            im: self % rhs.im,
        }
    }
}

impl Neg for Complex {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self {
            re: -self.re,
            im: -self.im,
        }
    }
}

impl PartialOrd for Complex {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.re
            .partial_cmp(&other.re)
            .or_else(|| self.im.partial_cmp(&other.im))
    }
}

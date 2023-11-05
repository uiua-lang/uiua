use std::{f64::consts::E, fmt, ops::*};

/// Uiua's complex number type
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Default)]
pub struct Complex {
    /// The real part
    pub re: f64,
    /// The imaginary part
    pub im: f64,
}

impl Complex {
    /// The complex number 0 + 0i
    pub const ZERO: Self = Self { re: 0.0, im: 0.0 };
    /// The complex number 1 + 0i
    pub const ONE: Self = Self { re: 1.0, im: 0.0 };
    /// The complex number 0 + 1i
    pub const I: Self = Self { re: 0.0, im: 1.0 };
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
    pub fn atan2(self, rhs: impl Into<Self>) -> Complex {
        let rhs = rhs.into();
        let denom = self.re * self.re + self.im * self.im;
        Self::new(
            (self.re * rhs.re + self.im * rhs.im) / denom,
            (self.im * rhs.re - self.re * rhs.im) / denom,
        )
    }
    /// Get the sign of a complex number
    pub fn signum(self) -> Self {
        Self::new(
            if self.re.is_nan() {
                f64::NAN
            } else if self.re == 0.0 {
                0.0
            } else {
                self.re.signum()
            },
            if self.im.is_nan() {
                f64::NAN
            } else if self.im == 0.0 {
                0.0
            } else {
                self.im.signum()
            },
        )
    }
    /// Calculate the principal value of the complex number
    pub fn arg(self) -> f64 {
        self.im.atan2(self.re)
    }
    /// Convert a complex number to polar coordinates
    pub fn to_polar(self) -> (f64, f64) {
        (self.abs(), self.arg())
    }
    /// Convert polar coordinates to a complex number
    pub fn from_polar(r: f64, theta: f64) -> Self {
        r * Self::new(theta.cos(), theta.sin())
    }
    /// Raise a complex number to a complex power
    pub fn powc(self, power: impl Into<Self>) -> Self {
        let power = power.into();
        if power.im == 0.0 {
            if self.im == 0.0 {
                return Self::new(self.re.powf(power.re), 0.0);
            }
            if power.re == 0.0 {
                return Self::ONE;
            }
        }
        let (r, theta) = self.to_polar();
        ((r.ln() + Self::I * theta) * power).exp()
    }
    /// Raise a complex number to a real power
    pub fn powf(self, power: f64) -> Self {
        if power == 0.0 {
            return Self::ONE;
        }
        if power.fract() == 0.0 {
            return Self::new(self.re.powf(power), self.im.powf(power));
        }
        let (r, theta) = self.to_polar();
        Self::from_polar(r.powf(power), theta * power)
    }
    /// Calculate the exponential of a complex number
    pub fn exp(self) -> Self {
        Self::from_polar(E.powf(self.re), self.im)
    }
    /// Calculate the natural logarithm of a complex number
    pub fn ln(self) -> Self {
        let (r, theta) = self.to_polar();
        Self::new(r.ln(), theta)
    }
    /// Calculate the logarithm of a complex number
    pub fn log(self, base: impl Into<Self>) -> Self {
        let base = base.into();
        Self::new(self.abs().ln(), self.arg()) / (Self::new(base.abs().ln(), base.arg()))
    }
    /// Calculate the square root of a complex number
    pub fn sqrt(self) -> Self {
        let (r, theta) = self.to_polar();
        Self::from_polar(r.sqrt(), theta / 2.0)
    }
    /// Calculate the sine of a complex number
    pub fn sin(self) -> Self {
        Self::new(
            self.re.sin() * self.im.cosh(),
            self.re.cos() * self.im.sinh(),
        )
    }
    /// Calculate the cosine of a complex number
    pub fn cos(self) -> Self {
        Self::new(
            self.re.cos() * self.im.cosh(),
            -self.re.sin() * self.im.sinh(),
        )
    }
    /// Calculate the arc sine of a complex number
    pub fn asin(self) -> Self {
        let z = Self::new(-self.im, self.re);
        (Self::ONE - (Self::ONE - self * self).sqrt()).log(z) / z
    }
    /// Calculate the arc cosine of a complex number
    pub fn acos(self) -> Self {
        let z = Self::new(-self.im, self.re);
        (z - (self - Self::ONE) * (self + Self::ONE).sqrt()).log(z) / z
    }
}

impl From<f64> for Complex {
    fn from(re: f64) -> Self {
        Self { re, im: 0.0 }
    }
}

impl From<u8> for Complex {
    fn from(value: u8) -> Self {
        f64::from(value).into()
    }
}

impl fmt::Display for Complex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.im == 0.0 {
            self.re.fmt(f)
        } else {
            write!(f, "{}r{}i", self.re, self.im)
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

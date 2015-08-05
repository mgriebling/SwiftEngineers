//
//  complex.swift
//
//	The MIT License (MIT)
//
//	Copyright (c) 2014 Dan Kogai
//  Copyright (c) 2015 Extensively modified by Michael Griebling
//
//	Permission is hereby granted, free of charge, to any person obtaining a copy
//	of this software and associated documentation files (the "Software"), to deal
//	in the Software without restriction, including without limitation the rights
//	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//	copies of the Software, and to permit persons to whom the Software is
//	furnished to do so, subject to the following conditions:
//
//	The above copyright notice and this permission notice shall be included in all
//	copies or substantial portions of the Software.
//
//	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//	SOFTWARE.

import Foundation

class BigComplex : BigReal, FloatLiteralConvertible, IntegerLiteralConvertible, StringLiteralConvertible {
	
	typealias ExtendedGraphemeClusterLiteralType = StringLiteralType
	typealias UnicodeScalarLiteralType = Character
	
    var im = BigReal()
	
    init(_ re:BigReal, _ im:BigReal) {
        self.im = im
		super.init(re.number)
    }
	
	init(_ re:BigReal) {
		im = BigReal.ZERO
		super.init(re.number)
	}

	init(_ re:Double, _ im:Double) {
		self.im = BigReal(im)
		super.init(re)
	}
	
	init(_ re:Int, _ im:Int) {
		self.im = BigReal(im)
		super.init(re)
	}
	
	convenience override init(_ re: Double) {
		self.init(floatLiteral: re)
	}
	
	convenience override init(_ re: Int) {
		self.init(integerLiteral: re)
	}
	
	convenience override init(_ s: String) {
		self.init(stringLiteral: s)
	}
	
	//
	// FloatLiteralConvertible protocol
	//
	required init(floatLiteral re:Double) {
		im = BigReal.ZERO
		super.init(re)
	}
	
	//
	// IntegerLiteralConvertible protocol
	//
	required init(integerLiteral re:Int) {
		im = BigReal.ZERO
		super.init(re)
	}
	
	//
	// StringLiteralConvertible protocol
	//
	required convenience init(extendedGraphemeClusterLiteral value: ExtendedGraphemeClusterLiteralType) {
		self.init(stringLiteral: value)
	}

	//
	// StringLiteralConvertible protocol
	//
	required convenience init(unicodeScalarLiteral value: UnicodeScalarLiteralType) {
		self.init(stringLiteral: "\(value)")
	}

	//
	// StringLiteralConvertible protocol
	//
	required init(stringLiteral s: String) {
		var vs = s.stringByReplacingOccurrencesOfString(" ", withString: "").lowercaseString  // remove all spaces & make lowercase
		if !vs.isEmpty {
			// break apart the string into real and imaginary pieces
			let signChars = NSCharacterSet(charactersInString: "+-")
			let exponent = "e"
			let imaginary = "i"
			var number = ""
			var inumber = ""
			var ch = vs[vs.startIndex]
			var iPresent = false
			
			// remove leading sign -- if any
			if signChars.characterIsMember(ch.toUnichar()) { number.append(ch); ch = vs.removeAtIndex(vs.startIndex) }
			if var range = vs.rangeOfCharacterFromSet(signChars) {
				// check if this is an exponent
				if let expRange = vs.rangeOfString(exponent) where expRange.startIndex == range.startIndex.predecessor() {
					// search beyond the exponent
					range = range.startIndex.successor()...vs.endIndex
					if let range = vs.rangeOfCharacterFromSet(signChars, options: [], range: range) {
						// This is likely the start of the second number
						number += vs.substringToIndex(range.startIndex)
						inumber = vs.substringFromIndex(range.startIndex)
					} else {
						// Only one number exists
						if let _ = vs.rangeOfString(imaginary) {
							inumber = number + vs 	// transfer the sign
							number = ""				// clear the real part
						} else {
							number += vs			// copy the number
						}
					}
				} else {
					// This is the start of the second number
					number += vs.substringToIndex(range.startIndex)
					inumber = vs.substringFromIndex(range.startIndex)
				}
			} else {
				// only one number exists
				if let _ = vs.rangeOfString(imaginary) {
					inumber = number + vs 	// transfer the sign
					number = ""				// clear the real part
				} else {
					number += vs			// copy the number
				}
			}
			super.init(number)
			iPresent = !inumber.isEmpty
			inumber = inumber.stringByReplacingOccurrencesOfString(imaginary, withString: "") // remove the "i"
			
			// account for solitary "i"
			if iPresent {
				if inumber.isEmpty { inumber = "1" }
				else if inumber == "+" || inumber == "-" { inumber += "1" }
			}
			im = BigReal(inumber)
		} else {
			super.init()
		}
	}
	
	override var isZero: Bool { return re.number.zero && im.number.zero }
	
    convenience override init() { self.init(0) }
	
    init(abs:BigReal, arg:BigReal) {
        im = abs * arg.sin()
		super.init((abs * arg.cos()).number)
    }
	
    /// real part thereof
    var real:BigReal { return re }
	
    /// imaginary part thereof
    var imag:BigReal { return im }
	
    /// absolute value thereof
    override var abs:BigReal {
        return re.hypot(im)
    }

	override func ipower(n: Int) -> BigComplex {
		var Z = BigComplex(0)
		var N = Swift.abs(n)
		var t : UInt
		var Y = BigComplex(1)
		
		if self.isZero {
			if n == 0 { return Y }
			return Z
		}
		
		Z = self
		while true {
			t = UInt(N % 2); N = N / 2
			if t != 0 {
				Y *= Z
			}
			if N == 0 { break }
			Z *= Z
		}

		if n < 0 { return 1/Y }
		return Y
	}
	
    /// argument thereof
    var arg:BigReal  {
        return im.atan2(re)
    }
	
    /// norm thereof
    var norm:BigReal { return re.hypot(im) }
	
    /// conjugate thereof
    var conj:BigComplex { return BigComplex(re, -im) }
	
    /// projection thereof
    var proj:BigComplex {
        if re.isFinite && im.isFinite {
            return self
        } else {
            return BigComplex(
                BigReal.ONE/BigReal(0), im.isSignMinus ? -BigReal(0) : BigReal(0)
            )
        }
    }
	
    /// (real, imag)
    var tuple:(BigReal, BigReal) {
        return (re, im)
    }
	
    /// z * i
	static var i: BigComplex { return BigComplex(0, 1) }
    var i:BigComplex { return BigComplex.i }
	
    /// .description -- conforms to Printable
    override var description:String {
        let plus = im.isSignMinus ? "" : "+"
		let ims = im == BigReal(1) ? "i" : im == BigReal(-1) ? "-i" : "\(im)i"
		if im.isZero { return re.description }
		if re.isZero { return "\(ims)" }
        return "\(re)\(plus)\(ims)"
    }
	
    /// .hashvalue -- conforms to Hashable
    override var hashValue:Int { // take most significant halves and join
        let bits = sizeof(Int) * 4
        let mask = bits == 16 ? 0xFFFF : 0xFFFF_FFFF
        return (re.hashValue & ~mask) | (im.hashValue >> bits)
    }
}

let i = BigComplex.i	// perhaps this is dangerous but very convenient -- Mike

// operator definitions
infix operator ** { associativity right precedence 170 }
infix operator **= { associativity right precedence 90 }
infix operator =~ { associativity none precedence 130 }
infix operator !~ { associativity none precedence 130 }

// != is auto-generated thanks to Equatable
func == (lhs:BigComplex, rhs:BigComplex) -> Bool {
    return lhs.re == rhs.re && lhs.im == rhs.im
}
func == (lhs:BigComplex, rhs:BigReal) -> Bool {
    return lhs.re == rhs && lhs.im == BigReal(0)
}
func == (lhs:BigReal, rhs:BigComplex) -> Bool {
    return rhs.re == lhs && rhs.im == BigReal(0)
}

// +, +=
prefix func + (z:BigComplex) -> BigComplex {
    return z
}
prefix func + (z:Double) -> BigComplex {
	return BigComplex(floatLiteral: z)
}
prefix func + (z:Int) -> BigComplex {
	return BigComplex(integerLiteral: z)
}
func + (lhs:BigComplex, rhs:BigComplex) -> BigComplex {
    return BigComplex(lhs.re + rhs.re, lhs.im + rhs.im)
}
func + (lhs:BigComplex, rhs:BigReal) -> BigComplex {
    return lhs + BigComplex(rhs)
}
func + (lhs:BigReal, rhs:BigComplex) -> BigComplex {
    return BigComplex(lhs) + rhs
}
func + (lhs:BigComplex, rhs:Double) -> BigComplex {
	return lhs + BigComplex(floatLiteral: rhs)
}
func + (lhs:Double, rhs:BigComplex) -> BigComplex {
	return BigComplex(floatLiteral: lhs) + rhs
}
func + (lhs:BigComplex, rhs:Int) -> BigComplex {
	return lhs + BigComplex(integerLiteral: rhs)
}
func + (lhs:Int, rhs:BigComplex) -> BigComplex {
	return BigComplex(integerLiteral: lhs) + rhs
}
func += (inout lhs:BigComplex, rhs:BigComplex) {
	lhs = BigComplex(lhs.re+rhs.re, lhs.im+rhs.im)
}
func += (inout lhs:BigComplex, rhs:BigReal) {
	lhs = BigComplex(lhs.re+rhs, lhs.im)
}

// -, -=
prefix func - (z:BigComplex) -> BigComplex {
    return BigComplex(-z.re, -z.im)
}
prefix func - (z:Double) -> BigComplex {
	return -BigComplex(floatLiteral: z)
}
prefix func - (z:Int) -> BigComplex {
	return BigComplex(-z)
}
func - (lhs:BigComplex, rhs:BigComplex) -> BigComplex {
    return BigComplex(lhs.re - rhs.re, lhs.im - rhs.im)
}
func - (lhs:BigComplex, rhs:BigReal) -> BigComplex {
    return lhs - BigComplex(rhs, BigReal(0))
}
func - (lhs:BigReal, rhs:BigComplex) -> BigComplex {
    return BigComplex(lhs, BigReal(0)) - rhs
}
func - (lhs:BigComplex, rhs:Double) -> BigComplex {
	return lhs - BigComplex(floatLiteral: rhs)
}
func - (lhs:Double, rhs:BigComplex) -> BigComplex {
	return BigComplex(floatLiteral: lhs) - rhs
}
func - (lhs:BigComplex, rhs:Int) -> BigComplex {
	return lhs - BigComplex(integerLiteral: rhs)
}
func - (lhs:Int, rhs:BigComplex) -> BigComplex {
	return BigComplex(integerLiteral: lhs) - rhs
}
func -= (inout lhs:BigComplex, rhs:BigComplex) {
	lhs = BigComplex(lhs.re - rhs.re, lhs.im - rhs.im)
}
func -= (inout lhs:BigComplex, rhs:BigReal) {
    lhs = BigComplex(lhs.re - rhs, lhs.im)
}

// *, *=
func * (lhs:BigComplex, rhs:BigComplex) -> BigComplex {
    return BigComplex(
        lhs.re * rhs.re - lhs.im * rhs.im,
        lhs.re * rhs.im + lhs.im * rhs.re
    )
}
func * (lhs:BigComplex, rhs:BigReal) -> BigComplex {
    return BigComplex(lhs.re * rhs, lhs.im * rhs)
}
func * (lhs:BigReal, rhs:BigComplex) -> BigComplex {
    return BigComplex(lhs * rhs.re, lhs * rhs.im)
}
func * (lhs:BigComplex, rhs:Double) -> BigComplex {
	return lhs * BigComplex(floatLiteral: rhs)
}
func * (lhs:Double, rhs:BigComplex) -> BigComplex {
	return BigComplex(floatLiteral: lhs) * rhs
}
func * (lhs:BigComplex, rhs:Int) -> BigComplex {
	return lhs * BigComplex(integerLiteral: rhs)
}
func * (lhs:Int, rhs:BigComplex) -> BigComplex {
	return BigComplex(integerLiteral: lhs) * rhs
}
func *= (inout lhs:BigComplex, rhs:BigComplex) {
    lhs = lhs * rhs
}
func *= (inout lhs:BigComplex, rhs:BigReal) {
    lhs = lhs * rhs
}

// /, /=
//
// cf. https://github.com/dankogai/swift-complex/issues/3
//
func / (lhs:BigComplex, rhs:BigComplex) -> BigComplex {
    if rhs.re.abs >= rhs.im.abs {
        let r = rhs.im / rhs.re
        let d = rhs.re + rhs.im * r
        return BigComplex (
            (lhs.re + lhs.im * r) / d,
            (lhs.im - lhs.re * r) / d
        )
    } else {
        let r = rhs.re / rhs.im
        let d = rhs.re * r + rhs.im
        return BigComplex (
            (lhs.re * r + lhs.im) / d,
            (lhs.im * r - lhs.re) / d
        )
        
    }
}
func / (lhs:BigComplex, rhs:BigReal) -> BigComplex {
    return BigComplex(lhs.re / rhs, lhs.im / rhs)
}
func / (lhs:BigReal, rhs:BigComplex) -> BigComplex {
    return BigComplex(lhs) / rhs
}
func /= (inout lhs:BigComplex, rhs:BigComplex) {
    lhs = lhs / rhs
}
func /= (inout lhs:BigComplex, rhs:BigReal) {
    lhs = lhs / rhs
}
func / (lhs:BigComplex, rhs:Double) -> BigComplex {
	return lhs / BigComplex(floatLiteral: rhs)
}
func / (lhs:Double, rhs:BigComplex) -> BigComplex {
	return BigComplex(floatLiteral: lhs) / rhs
}
func / (lhs:BigComplex, rhs:Int) -> BigComplex {
	return lhs / BigComplex(integerLiteral: rhs)
}
func / (lhs:Int, rhs:BigComplex) -> BigComplex {
	return BigComplex(integerLiteral: lhs) / rhs
}

// exp(z)
func exp(z:BigComplex) -> BigComplex {
    let abs = z.re.exp()
    let arg = z.im
    return BigComplex(abs * arg.cos(), abs * arg.sin())
}

// ln(z)
func ln(z:BigComplex) -> BigComplex {
    return BigComplex(z.abs.ln(), z.arg)
}

// log(z) -- just because C++ has it
func log(z:BigComplex) -> BigComplex { return ln(z) / BigReal.LN10 }
func log(r:BigReal) -> BigComplex { return BigComplex(r.log()) }

// pow(b, x)
func pow(lhs:BigComplex, rhs:BigComplex) -> BigComplex {
    if lhs.isZero { return BigComplex(BigReal.ONE) } // 0 ** 0 == 1
	if rhs.im.isZero && rhs.re.isInteger() {
		return lhs.ipower(rhs.re.integer)
	}
    let z = ln(lhs) * rhs
    return exp(z)
}
func pow(lhs:BigComplex, rhs:BigReal) -> BigComplex {
    return pow(lhs, rhs: BigComplex(rhs, BigReal(0)))
}
func pow(lhs:BigReal, rhs:BigComplex) -> BigComplex {
    return pow(BigComplex(lhs, BigReal(0)), rhs: rhs)
}

// **, **=
func ** (lhs:BigReal, rhs:BigReal) -> BigComplex {
    return BigComplex(lhs.pow(rhs))
}
func ** (lhs:BigComplex, rhs:BigComplex) -> BigComplex {
    return pow(lhs, rhs: rhs)
}
func ** (lhs:BigReal, rhs:BigComplex) -> BigComplex {
    return pow(lhs, rhs: rhs)
}
func ** (lhs:BigComplex, rhs:BigReal) -> BigComplex {
    return pow(lhs, rhs: rhs)
}
func ** (lhs:BigComplex, rhs:Double) -> BigComplex {
	return lhs ** BigComplex(floatLiteral: rhs)
}
func ** (lhs:Double, rhs:BigComplex) -> BigComplex {
	return BigComplex(floatLiteral: lhs) ** rhs
}
func ** (lhs:BigComplex, rhs:Int) -> BigComplex {
	return lhs ** BigComplex(integerLiteral: rhs)
}
func ** (lhs:Int, rhs:BigComplex) -> BigComplex {
	return BigComplex(integerLiteral: lhs) ** rhs
}
func ** (lhs:Int, rhs:Int) -> BigComplex {
	return BigComplex(integerLiteral: lhs) ** BigComplex(integerLiteral: rhs)
}
func **= (inout lhs:BigReal, rhs:BigReal) {
    lhs = lhs.pow(rhs)
}
func **= (inout lhs:BigComplex, rhs:BigComplex) {
    lhs = pow(lhs, rhs: rhs)
}
func **= (inout lhs:BigComplex, rhs:BigReal) {
    lhs = pow(lhs, rhs: rhs)
}

// sqrt(z)
func sqrt(z:BigComplex) -> BigComplex {
    // return z ** 0.5
    let d = z.re.hypot(z.im)
    let re = ((z.re + d)/BigReal.TWO).sqrt()
    if z.im < BigReal(0) {
        return BigComplex(re, -((-z.re + d)/BigReal.TWO).sqrt())
    } else {
        return BigComplex(re,  ((-z.re + d)/BigReal.TWO).sqrt())
    }
}

// cos(z)
func cos(z:BigComplex) -> BigComplex {
    // return (exp(i*z) + exp(-i*z)) / 2
    return (exp(z.i) + exp(-z.i)) / BigReal.TWO
}

// sin(z)
func sin(z:BigComplex) -> BigComplex {
    // return (exp(i*z) - exp(-i*z)) / (2*i)
    return -(exp(z.i) - exp(-z.i)).i / BigReal.TWO
}

// tan(z)
func tan(z:BigComplex) -> BigComplex {
    // return sin(z) / cos(z)
    let ezi = exp(z.i), e_zi = exp(-z.i)
    return (ezi - e_zi) / (ezi + e_zi).i
}

// atan(z)
func atan(z:BigComplex) -> BigComplex {
    let l0 = log(BigReal.ONE - z.i), l1 = log(BigReal.ONE + z.i)
    return (l0 - l1).i / BigReal.TWO
}

func atan(r:BigReal) -> BigComplex { return atan(BigComplex(r)) }

// atan2(z, zz)
func atan2(z:BigComplex, zz:BigComplex) -> BigComplex {
    return atan(z / zz)
}

// asin(z)
func asin(z:BigComplex) -> BigComplex {
    return -log(z.i + sqrt(BigReal.ONE - z*z)).i
}

// acos(z)
func acos(z:BigComplex) -> BigComplex {
    return log(z - sqrt(BigReal.ONE - z*z).i).i
}

// sinh(z)
func sinh(z:BigComplex) -> BigComplex {
    return (exp(z) - exp(-z)) / BigReal.TWO
}

// cosh(z)
func cosh(z:BigComplex) -> BigComplex {
    return (exp(z) + exp(-z)) / BigReal.TWO
}

// tanh(z)
func tanh(z:BigComplex) -> BigComplex {
    let ez = exp(z), e_z = exp(-z)
    return (ez - e_z) / (ez + e_z)
}

// asinh(z)
func asinh(z:BigComplex) -> BigComplex {
    return ln(z + sqrt(z*z + BigReal.ONE))
}

// acosh(z)
func acosh(z:BigComplex) -> BigComplex {
    return ln(z + sqrt(z*z - BigReal.ONE))
}

// atanh(z)
func atanh(z:BigComplex) -> BigComplex {
    let t = ln((BigReal.ONE + z)/(BigReal.ONE - z))
    return t / BigReal.TWO
}

// for the compatibility's sake w/ C++11
func abs(z:BigComplex) -> BigReal { return z.abs }
func arg(z:BigComplex) -> BigReal { return z.arg }
func real(z:BigComplex) -> BigReal { return z.real }
func imag(z:BigComplex) -> BigReal { return z.imag }
func norm(z:BigComplex) -> BigReal { return z.norm }
func conj(z:BigComplex) -> BigComplex { return z.conj }
func proj(z:BigComplex) -> BigComplex { return z.proj }

//
// approximate comparisons
//
func =~ (lhs:BigReal, rhs:BigReal) -> Bool {
    if lhs == rhs { return true }
    let t = (rhs - lhs) / rhs
    return t.abs <= BigReal.TWO * BigReal.epsilon
}
func =~ (lhs:BigComplex, rhs:BigComplex) -> Bool {
    if lhs == rhs { return true }
    return lhs.abs =~ rhs.abs
}
func =~ (lhs:BigComplex, rhs:BigReal) -> Bool {
    return lhs.abs =~ rhs.abs
}
func =~ (lhs:BigReal, rhs:BigComplex) -> Bool {
    return lhs.abs =~ rhs.abs
}
func !~ (lhs:BigReal, rhs:BigReal) -> Bool {
    return !(lhs =~ rhs)
}
func !~ (lhs:BigComplex, rhs:BigComplex) -> Bool {
    return !(lhs =~ rhs)
}
func !~ (lhs:BigComplex, rhs:BigReal) -> Bool {
    return !(lhs =~ rhs)
}
func !~ (lhs:BigReal, rhs:BigComplex) -> Bool {
    return !(lhs =~ rhs)
}

extension Double {
	var i : BigComplex {
		return self * BigComplex.i
	}
}

extension Int {
	var i : BigComplex {
		return self * BigComplex.i
	}
}



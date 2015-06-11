//
//  Equations.swift
//  TestEngineerMath
//
//  Created by Mike Griebling on 18 May 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Foundation

class Equations {
	
	static func solveUsingCramersRule (a: Matrix, y: Vector) -> Vector? {
		let n = y.count
		let det = a.determinant()
		var coef = Vector(size:n)
		
		func computeCramerForColumn (col: Int) -> Double {
			if let b = a.replaceColumn(col, withVector: y) {
				return b.determinant()/det
			}
			return 0
		}
		
		// sanity check for singular matrices and incorrect matrix/vector sizes
		if det == 0 || a.rows != n { return nil }
		
		// replace each column in matrix a with y and compute the determinant
		for col in 0..<n {
			coef[col] = computeCramerForColumn(col)
		}
		return coef
	}
	
	static func curveFit (x: Vector, y: Vector, order: Int) -> Vector? {
		let size = order + 1
		var xData = [Double](count: 2*order+1, repeatedValue: 0)
		var yData = [Double](count: size, repeatedValue: 0)
		var b = x
		var c = y

		// compute the sums of all orders of x
		xData[0] = Double(x.count)
		for i in 1...2*order {
			xData[i] = b.sum()
			b = b.mul(x)
		}
		
		// compute the sums of all orders of y
		for i in 0...order {
			yData[i] = c.sum()
			c = c.mul(x)
		}
		
		// create a matrix holding the correct orders
		var matrix = [Double](count: size*size, repeatedValue: 0)
		for i in 0...order {
			for j in 0...order {
				matrix[i*size+j] = xData[i+j]
			}
		}
		
		// compute the curve-fitting coefficients by solving these equations
		let m = Matrix(matrix, numberOfRows: size)
		if let result = solveUsingCramersRule(m, y: Vector(yData)) { // Matrix.solveLinear1(matrix, n: yData) {
			return result
		}
		return nil
	}
	
	static func solveUsingGaussianElimination (a: Matrix, y: Vector) -> Vector? {
		let n = y.count-1
		let b = a
		let w = y
		let coeff = Vector(size: y.count)
		
		for i in 0...n-1 {
			// find largest pivot point
			var big = abs(b[i,i])
			var l = i
			let i1 = i+1
			
			for j in i1...n {
				let ab = abs(b[i,j])
				if ab > big { big = ab; l = j }
			}
			
			if big == 0 { return nil }
			
			if l != i {
				// interchange rows to put the largest element on diagonal
				let rowl = b.getRow(l)
				b.putRow(l, r: b.getRow(i))
				b.putRow(i, r: rowl)
				let wl = w[l]
				w[l] = w[i]
				w[i] = wl
			}
			
			// scale and subtract equations
			for j in i1...n {
				let t = b[i,j] / b[i,i]
				for k in i1...n {
					b[k,j] = b[k,j] - t * b[k,i]
				}
				w[j] = w[j] - t * w[i]
			}
		}
		
		if b[n,n] == 0 { return nil }
		
		// back substitution to determine other values
		coeff[n] = w[n] / b[n,n]
		var i = n - 1
		
		repeat {
			var sum = 0.0
			for j in i+1...n {
				sum += b[j,i] * coeff[j]
			}
			coeff[i] = (w[i] - sum) / b[i,i]
			i--
		} while i >= 0
		
		return coeff
	}

}
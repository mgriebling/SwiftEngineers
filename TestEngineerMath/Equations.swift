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
	
	static func solveUsingGaussianElimination (a: Matrix, y: Vector) -> Vector? {
		let n = y.count
		var b = a
		
		for i in 0..<n-1 {
			// find largest pivot point
			var big = b[i, i]
			var l = i
			var i1 = i+1
			
			for j in i1..<n {
				var ab = b[j, i]
				if ab > big { big = ab; l = j }
			}
			
			if big == 0 { return nil }
			
			if l != i {
				// interchange rows to put the largest element on diagonal
				let rowl = b.getRow(l)
				b.putRow(l, r: b.getRow(i))
				b.putRow(i, r: rowl)
				
				// scale
			}
		}
		
		return nil
	}

}
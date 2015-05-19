//
//  MatrixVector.swift
//  TestEngineerMath
//
//  Created by Mike Griebling on 17 May 2015.
//  Copyright (c) 2015 Computer Inspirations. All rights reserved.
//

import Accelerate

class Vector : Printable {
	
	private var vector: [Double]
	
	init (size: Int) {
		vector = [Double](count: size, repeatedValue: 0)
	}
	
	init (_ v: [Double]) {
		vector = v
	}
	
	var count: Int {
		return vector.count
	}
	
	var description: String {
		return vector.description
	}
	
	subscript (i: Int) -> Double {
		get {
			if i < vector.count {
				return vector[i]
			}
			return 0
		}
		set (new) {
			if i < vector.count {
				vector[i] = new
			}
		}
	}
	
	static func add (v: [Double], w: Double) -> [Double] {
		var result = [Double](count: v.count, repeatedValue: 0)
		var lw = w
		vDSP_vsaddD(v, 1, &lw, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func mul (v: [Double], w: Double) -> [Double] {
		var result = [Double](count: v.count, repeatedValue: 0)
		var lw = w
		vDSP_vsmulD(v, 1, &lw, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func div (v: [Double], w: Double) -> [Double] {
		var result = [Double](count: v.count, repeatedValue: 0)
		var lw = w
		vDSP_vsdivD(v, 1, &lw, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func add (v: [Double], w: [Double]) -> [Double] {
		var result = [Double](count: v.count, repeatedValue: 0)
		vDSP_vaddD(v, 1, w, 1, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func neg (v: [Double]) -> [Double] {
		var result = [Double](count: v.count, repeatedValue: 0)
		vDSP_vnegD(v, 1, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func abs (v: [Double]) -> [Double] {
		var result = [Double](count: v.count, repeatedValue: 0)
		vDSP_vabsD(v, 1, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func square (v: [Double]) -> [Double] {
		var result = [Double](count: v.count, repeatedValue: 0)
		vDSP_vssqD(v, 1, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func mul (v: [Double], w: [Double]) -> [Double] {
		var result = [Double](count: v.count, repeatedValue: 0)
		vDSP_vmulD(v, 1, w, 1, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func div (v: [Double], w: [Double]) -> [Double] {
		var result = [Double](count: v.count, repeatedValue: 0)
		vDSP_vdivD(v, 1, w, 1, &result, 1, vDSP_Length(v.count))
		return result
	}
	
	static func dot (v: [Double], w: [Double]) -> Double {
		var result = 0.0
		vDSP_dotprD(v, 1, w, 1, &result, vDSP_Length(v.count))
		return result
	}
	
	static func sort (inout v: [Double], ascending: Bool) {
		vDSP_vsortD(&v, vDSP_Length(v.count), ascending ? 1 : -1)
	}
	
	static func max (v: [Double]) -> Double {
		var result = 0.0
		vDSP_maxvD(v, 1, &result, vDSP_Length(v.count))
		return result
	}
	
	static func min (v: [Double]) -> Double {
		var result = 0.0
		vDSP_minvD(v, 1, &result, vDSP_Length(v.count))
		return result
	}
	
	static func mean (v: [Double]) -> Double {
		var result = 0.0
		vDSP_meanvD(v, 1, &result, vDSP_Length(v.count))
		return result
	}
	
	static func rms (v: [Double]) -> Double {
		var result = 0.0
		vDSP_rmsqvD(v, 1, &result, vDSP_Length(v.count))
		return result
	}
	
	static func sum (v: [Double]) -> Double {
		var result = 0.0
		vDSP_sveD(v, 1, &result, vDSP_Length(v.count))
		return result
	}
	
	static func sumSquared (v: [Double]) -> Double {
		var result = 0.0
		vDSP_svesqD(v, 1, &result, vDSP_Length(v.count))
		return result
	}
	
}

class Matrix {
	
	private var matrix: [Double]
	private var irows: Int
	var columns: Int {
		return matrix.count / rows
	}
	var rows: Int {
		return irows
	}
	var count: Int {
		return matrix.count
	}
	
	init (_ a: [Double], numberOfRows: Int) {
		matrix = a
		irows = numberOfRows
	}
	
	subscript (col: Int, row: Int) -> Double {
		get {
			let index = row * columns + col
			if index < matrix.count {
				return matrix[index]
			}
			return 0
		}
		set (new) {
			let index = row * columns + col
			if index < matrix.count {
				matrix[index] = new
			}
		}
	}
	
	func add (matrix: Matrix) -> Matrix {
		return Matrix(Matrix.add(self.matrix, n: matrix.matrix), numberOfRows: irows)
	}
	
	func getRow (row: Int) -> Vector {
		var r = [Double]()
		for i in 0..<columns { r.append(self[i, row]) }
		return Vector(r)
	}
	
	func putRow (row: Int, r: Vector) {
		for i in 0..<columns { self[i, row] = r[i] }
	}
	
	func determinant () -> Double {
		return Matrix.determinant(matrix)
	}
	
	func replaceColumn (col: Int, withVector v: Vector) -> Matrix? {
		var result = matrix
		if v.count != irows { return nil }
		cblas_ccopy(Int32(v.count), v.vector, 1, &result[col], Int32(irows))
		return Matrix(result, numberOfRows: irows)
	}
	
	static func add (m: [Double], n: [Double]) -> [Double] {
		// same as vector add
		var result = [Double](count: m.count, repeatedValue: 0)
		vDSP_vaddD(m, 1, n, 1, &result, 1, vDSP_Length(m.count))
		return result
	}
	
	static func mul (m: [Double], mRows: Int, n: [Double], nCols: Int) -> [Double] {
		var result = [Double](count: mRows*nCols, repeatedValue: 0)
		vDSP_mmulD(m, 1, n, 1, &result, 1, vDSP_Length(mRows), vDSP_Length(nCols), vDSP_Length(m.count/mRows))
		return result
	}
	
	static func transpose (m: [Double], mRows: Int) -> [Double] {
		var result = [Double](count: m.count, repeatedValue: 0)
		vDSP_mtransD(m, 1, &result, 1, vDSP_Length(m.count/mRows), vDSP_Length(mRows))
		return result
	}
	
	static func invert (m: [Double]) -> [Double]? {
		var inMatrix = m
		var N = __CLPK_integer(sqrt(Double(m.count)))
		var pivots = [__CLPK_integer](count: Int(N), repeatedValue: 0)
		var workspace = [Double](count: Int(N), repeatedValue: 0)
		var error : __CLPK_integer = 0
		
		dgetrf_(&N, &N, &inMatrix, &N, &pivots, &error)
		if error != 0 { return nil }
		dgetri_(&N, &inMatrix, &N, &pivots, &workspace, &N, &error)
		if error != 0 { return nil }
		return inMatrix
	}
	
	static func determinant (m: [Double]) -> Double {
		var result = 1.0
		var neg = false
		var error : __CLPK_integer = 0
		var N = __CLPK_integer(sqrt(Double(m.count)))
		var pivots = [__CLPK_integer](count: Int(N), repeatedValue: 0)
		var workspace = [Double](count: Int(N), repeatedValue: 0)
		var tmp = m
		
		dgetrf_(&N, &N, &tmp, &N, &pivots, &error)
		if error != 0 { return 0 } // singular matrix
		
		// Take the product of the diagonal elements
		for i in 0..<Int(N) {
			result *= tmp[i+i*Int(N)]
			if pivots[i] != (i+1) { neg = !neg }
		}
		
		// Return the correct sign
		return neg ? -result : result
	}
}


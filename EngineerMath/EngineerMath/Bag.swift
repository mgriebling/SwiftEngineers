//
//  Bag.swift
//  EngineerMath
//
//  Created by Michael Griebling on 3Jun2015.
//  Copyright (c) 2015 Solinst Canada. All rights reserved.
//

import Foundation

public struct Bag<T:Hashable> : SequenceType, Equatable {
	
	var items = [T: Int]()	// Bag definition as an object and a quantity
	
	var count : Int {
		var itemCount = 0
		for item in items { itemCount += item.1 }
		return itemCount
	}
	
	var uniqueCount : Int {
		return items.count
	}
	
	subscript (item: T) -> Int {
		return items[item] ?? 0
	}
	
	mutating func add (item: T) {
		let value = items[item] ?? 0
		items[item] = value+1
	}
	
	mutating func removeOne (item: T) {
		let value = items[item] ?? 0
		if value == 0 { return }
		if value == 1 {
			items.removeValueForKey(item)
		} else {
			items[item] = value-1
		}
	}
	
	mutating func removeAll (item: T) {
		items.removeValueForKey(item)
	}
	
	mutating func removeAllItems () {
		items = [T: Int]()
	}
	
	func combinedWith (bag: Bag<T>) -> Bag<T> {
		var combined = bag
		for item in self {
			let value = (combined.items[item.0] ?? 0) + item.1
			combined.items[item.0] = value
		}
		return combined
	}
	
	func removeItemsIn (bag: Bag<T>) -> Bag<T> {
		var removed = self
		for item in bag {
			if let value = removed.items[item.0] {
				if item.1 < value {
					removed.items[item.0] = value - item.1
				} else {
					removed.removeAll(item.0)
				}
			}
		}
		return removed
	}
	
	func itemsAlsoIn (bag: Bag<T>) -> Bag<T> {
		var intersect = Bag<T>()
		for item in bag {
			if let value = self.items[item.0] {
				intersect.items[item.0] = min(value, item.1)
			}
		}
		return intersect
	}
	
	func isEmpty () -> Bool {
		return count == 0
	}
	
	// Sequence generator
	public func generate() -> DictionaryGenerator<T,Int> {
		return items.generate()
	}
	
	func isEqual (rhs: Bag<T>) -> Bool {
		if self.count != rhs.count { return false }
		if self.uniqueCount != rhs.uniqueCount { return false }
		for item in self {
			if item.1 != rhs[item.0] { return false }
		}
		return true
	}
	
}

public func == <T>(lhs: Bag<T>, rhs: Bag<T>) -> Bool {
	return lhs.isEqual(rhs)
}



//
//  Bag.swift
//  EngineerMath
//
//  Created by Michael Griebling on 3Jun2015.
//  Copyright (c) 2015 Solinst Canada. All rights reserved.
//

import Foundation

struct Bag<T:Hashable> : SequenceType {
	
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
		let value = items[item] ?? 0
		if value == 0 { return }
		items.removeValueForKey(item)
	}
	
	mutating func removeAllItems () {
		items = [T: Int]()
	}
	
	func isEmpty () -> Bool {
		return count == 0
	}
	
	// Sequence generator
	func generate() -> DictionaryGenerator<T,Int> {
		return items.generate()
	}
	
}

typealias UnitBag = Bag<String>


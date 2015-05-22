//
//  gmpBridge.h
//  EngineerMath
//
//  Created by Michael Griebling on 22May2015.
//  Copyright (c) 2015 Solinst Canada. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "gmp.h"

@interface gmp : NSObject

+ (NSInteger) getBitSize;
+ (void) setBitSize: (NSInteger)digits;
+ (void) setNumber: (mpf_t *)number fromString: (NSString *)string;
+ (void) setNumber: (mpf_t *)number fromInt: (NSInteger)integer;
+ (void) setNumber: (mpf_t *)number fromDouble: (double)x;
+ (void) setNumber: (mpf_t *)number fromLongDouble: (mpf_t)x;
+ (NSString *) getStringFrom: (const mpf_t)number;
+ (double) getDoubleFrom: (const mpf_t)number;
+ (NSInteger) getIntegerFrom: (const mpf_t)number;
+ (void) add: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result;
+ (void) sub: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result;
+ (void) mul: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result;
+ (void) div: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result;
+ (void) sqrt: (mpf_t)x giving: (mpf_t*)result;
+ (void) negate: (mpf_t)x giving: (mpf_t*)result;
+ (void) abs: (mpf_t)x giving: (mpf_t*)result;
+ (void) ipower:(mpf_t)x toPower:(NSInteger)power giving: (mpf_t*)result;
+ (NSComparisonResult) cmp:(mpf_t)x toNumber:(mpf_t)y;

@end

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

+ (NSInteger) getDigits;
+ (void) setDigits: (NSInteger)digits;
+ (void) setNumber: (mpf_t *)number fromString: (NSString *)string;
+ (NSString *) getStringFrom: (mpf_t)number;
+ (void) add: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result;
+ (void) sub: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result;
+ (void) mul: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result;
+ (void) div: (mpf_t)x toNumber: (mpf_t)y giving: (mpf_t*)result;

@end

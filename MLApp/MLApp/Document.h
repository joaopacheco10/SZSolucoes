//
//  Document.h
//  MLApp
//
//  Created by SZ Solucoes on 30/10/13.
//  Copyright (c) 2013 SZ Solucoes. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface Document : NSObject

@property (nonatomic, strong) NSString *docType;
@property (nonatomic) NSInteger *docNum;
@property (nonatomic) float *value;
@property (nonatomic, strong) NSDate *date;

@end

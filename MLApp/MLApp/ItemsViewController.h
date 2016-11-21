//
//  ItemsViewController.h
//  MLApp
//
//  Created by SZ Solucoes on 16/04/14.
//  Copyright (c) 2014 SZ Solucoes. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface ItemsViewController : UITableViewController
@property (nonatomic,strong) NSMutableArray *listOfItems;
@property (nonatomic,strong) NSString *docType;

@end

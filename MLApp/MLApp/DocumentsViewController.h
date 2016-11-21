//
//  DocumentsViewController.h
//  MLApp
//
//  Created by SZ Solucoes on 30/10/13.
//  Copyright (c) 2013 SZ Solucoes. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface DocumentsViewController : UITableViewController

@property (nonatomic,strong) NSArray *listOfDocs;
@property (nonatomic,strong) NSArray *listOfAgrees;
@property (nonatomic,strong) NSString *docType;

@end

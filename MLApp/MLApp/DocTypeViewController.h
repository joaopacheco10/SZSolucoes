//
//  DocTypeViewController.h
//  MLApp
//
//  Created by SZ Solucoes on 08/10/13.
//  Copyright (c) 2013 SZ Solucoes. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface DocTypeViewController : UITableViewController <UITableViewDataSource>

@property (weak, nonatomic) IBOutlet UIBarButtonItem *btSair;
@property (strong, nonatomic) NSArray *docTypes;
@property (strong, nonatomic) NSArray *idxType;

- (IBAction)btSairTap:(id)sender;
- (void) doDocumentsRequest;
@end

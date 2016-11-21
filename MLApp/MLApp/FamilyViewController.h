//
//  FamilyViewController.h
//  MLApp
//
//  Created by Joao Pacheco on 2/4/15.
//  Copyright (c) 2015 SZ Solucoes. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface FamilyViewController : UITableViewController

@property (nonatomic,strong) NSArray *listFamily;
@property (nonatomic,strong) NSString *docKey;
@property (weak,nonatomic) NSDictionary *doc;

- (IBAction)doApproval:(id)sender;
- (IBAction)doReject:(id)sender;

@property (weak, nonatomic) IBOutlet UIBarButtonItem *btnApproval;
@property (weak, nonatomic) IBOutlet UIBarButtonItem *btnReject;

@end

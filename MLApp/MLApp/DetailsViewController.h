//
//  DetailsViewController.h
//  MLApp
//
//  Created by SZ Solucoes on 15/04/14.
//  Copyright (c) 2014 SZ Solucoes. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface DetailsViewController : UIViewController
@property (weak, nonatomic) IBOutlet UITextField *txtFornec;
@property (weak, nonatomic) IBOutlet UITextField *txtData;
@property (weak, nonatomic) IBOutlet UITextField *txtSolic;
@property (weak, nonatomic) IBOutlet UITextField *txtCCusto;
@property (weak, nonatomic) IBOutlet UITextField *txtCodLotac;
@property (weak, nonatomic) IBOutlet UITextField *txtDesLotac;

@property (weak, nonatomic) IBOutlet UITextField *txtTotal;
@property (weak, nonatomic) IBOutlet UITextView *txtObs;
@property (weak, nonatomic) IBOutlet UITableViewCell *itemsCell;
- (IBAction)itemTap:(id)sender;
@property (weak, nonatomic) IBOutlet UISwitch *isAlternative;
@property (weak, nonatomic) IBOutlet UITextField *txtDtVencto;
@property (weak, nonatomic) IBOutlet UITextField *docKey;


@property (weak,nonatomic) NSDictionary *doc;
@property (nonatomic,strong) NSArray *family;

- (IBAction)doApprovalDet:(id)sender;
- (IBAction)doRejectDet:(id)sender;

@property (weak, nonatomic) IBOutlet UIBarButtonItem *btApprovalDet;
@property (weak, nonatomic) IBOutlet UIBarButtonItem *btRejectDet;

@end

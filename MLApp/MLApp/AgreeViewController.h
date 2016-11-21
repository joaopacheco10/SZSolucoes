//
//  AgreeViewController.h
//  MLApp
//
//  Created by Joao Pacheco on 17/10/16.
//  Copyright © 2016 SZ Solucoes. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface AgreeViewController : UIViewController
@property (weak, nonatomic) IBOutlet UITextField *numAgree;
@property (weak, nonatomic) IBOutlet UITextField *nomCust;
@property (weak, nonatomic) IBOutlet UITextField *dtPayment;
@property (weak, nonatomic) IBOutlet UITextField *valAgree;
@property (weak, nonatomic) IBOutlet UITextField *valInvest;
@property (weak, nonatomic) IBOutlet UITextField *valOrder;
@property (weak, nonatomic) IBOutlet UITextField *nomCondPag;
@property (weak, nonatomic) IBOutlet UITextField *numParc;
@property (weak, nonatomic) IBOutlet UITextField *nomRepres;
@property (weak, nonatomic) IBOutlet UITextField *nomArea;
@property (weak, nonatomic) IBOutlet UITextField *nomCanal;
@property (weak, nonatomic) IBOutlet UITextField *desAction;
@property (weak, nonatomic) IBOutlet UITextView *desObj;
@property (weak, nonatomic) IBOutlet UITextField *perIniAgree;
@property (weak, nonatomic) IBOutlet UITextField *perFimAgree;

@property (weak, nonatomic) IBOutlet UIBarButtonItem *btAprov;
@property (weak, nonatomic) IBOutlet UIBarButtonItem *btReprov;

- (IBAction)doAprov:(id)sender;
- (IBAction)doReprov:(id)sender;

@property (weak,nonatomic) NSDictionary *agree;

@end

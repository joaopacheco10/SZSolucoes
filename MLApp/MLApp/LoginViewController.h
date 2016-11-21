//
//  MLAppViewController.h
//  MLApp
//
//  Created by SZ Solucoes on 07/10/13.
//  Copyright (c) 2013 SZ Solucoes. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "MBProgressHUD.h"

@interface LoginViewController : UIViewController <UITextFieldDelegate, MBProgressHUDDelegate, UIPickerViewDataSource, UIPickerViewDelegate, NSURLSessionDelegate>

- (IBAction)doLogin:(id) sender;
- (IBAction)doLoadPicker:(id)sender;

@property (weak, nonatomic) IBOutlet UIButton *btLogin;
@property (weak, nonatomic) IBOutlet UITextField *txtUsuario;
@property (weak, nonatomic) IBOutlet UITextField *txtSenha;
@property (strong, nonatomic) IBOutlet UIScrollView *scrollView;
@property (weak, nonatomic) IBOutlet UIPickerView *pickerComp;
@property (weak, nonatomic) IBOutlet UIButton *btPickerLoad;
@property (strong, nonatomic) IBOutlet UIView *pickerView;

@end

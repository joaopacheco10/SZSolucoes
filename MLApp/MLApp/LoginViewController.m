//
//  MLAppViewController.m
//  MLApp
//
//  Created by SZ Solucoes on 07/10/13.
//  Copyright (c) 2013 SZ Solucoes. All rights reserved.
//

#import "LoginViewController.h"
#import "MBProgressHUD.h"

@interface LoginViewController () <UITextFieldDelegate>

{
    NSDictionary *company;
    NSArray *pickerData;
}

@end

@implementation LoginViewController

@synthesize txtUsuario;
@synthesize txtSenha;
@synthesize scrollView;
@synthesize pickerComp;
@synthesize btPickerLoad;
@synthesize btLogin;
@synthesize pickerView;

UITextField *activeField = nil;

- (void)viewDidLoad
{
    [super viewDidLoad];
    // Do any additional setup after loading the view, typically from a nib.
    
    NSString *userName = [[NSUserDefaults standardUserDefaults] stringForKey:@"UserName"];
    NSString *password = [[NSUserDefaults standardUserDefaults] stringForKey:@"Password"];
    
    txtUsuario.text = userName;
    txtUsuario.delegate = self;
    
    txtSenha.text = password;
    txtSenha.delegate = self;
    
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (IBAction)doLogin:(id)sender {
    
    MBProgressHUD *HUD = [MBProgressHUD showHUDAddedTo:self.view animated:YES];
    [self.view addSubview:HUD];
    //HUD.dimBackground = YES;
    HUD.delegate = self;
    //HUD.color = [UIColor colorWithRed:(218/255.0) green:(83/255.0) blue:(32/255.0) alpha:1];
    HUD.labelText = @"Conectando...";
    
    HUD.removeFromSuperViewOnHide = YES;
    
    dispatch_time_t popTime = dispatch_time(DISPATCH_TIME_NOW, 0.01 * NSEC_PER_SEC);
    dispatch_after(popTime, dispatch_get_main_queue(), ^(void){
        NSString *result = [self performLogin];
        
        if ([result isEqualToString:@"OK"]) {
            
            [[NSUserDefaults standardUserDefaults] setObject:txtUsuario.text forKey:@"UserName"];
            [[NSUserDefaults standardUserDefaults] setObject:txtSenha.text forKey:@"Password"];
            
            NSString *appType = [[NSUserDefaults standardUserDefaults] stringForKey:@"appType"];
            
            if ([company count] < 2) {
                
                if ([appType isEqualToString:@"Familia"]) {
                    [self performSegueWithIdentifier:@"ShowMainViewFam" sender:self];
                }
                else {
                    [self performSegueWithIdentifier:@"ShowMainViewDet" sender:self];
                }
            } else {
                
                [HUD hide:YES];
                
                [self showPicker];
            }
            
            //[MBProgressHUD hideHUDForView:self.view animated:YES];
        }
        else {
            
            [HUD hide:YES];
            
            
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Erro"
                                                            message:[NSString stringWithFormat:@"Erro ao efetuar login: %@",result ]
                                                           delegate:self
                                                  cancelButtonTitle:@"OK"
                                                  otherButtonTitles: nil];
            
            [alert show];
            return ;
            
        }
    });
}

- (void)showPicker
{
    
    if ([company count] > 1) {
        
        [txtSenha setHidden:true];
        [txtUsuario setHidden:true];
        [btLogin setHidden:true];
        
        pickerData = [NSArray arrayWithObject:[company valueForKey:@"name"]];
        
        pickerData = pickerData[0];
        
        //[btPickerLoad setHidden:false];
        
        self.pickerComp.delegate = self;
        self.pickerComp.dataSource = self;
        [self.pickerComp reloadAllComponents];
        
        [pickerComp setHidden:false];
        [btPickerLoad setHidden:false];
        [pickerView setHidden:false];
        
        
    }
    
}

- (IBAction)doLoadPicker:(id)sender {
    
    NSInteger row;
    
    NSArray *compData = [NSArray arrayWithObject:company];
    
    compData = compData[0];
    
    row = [pickerComp selectedRowInComponent:0];
    
    NSString *appType = [[NSUserDefaults standardUserDefaults] stringForKey:@"appType"];
    
    NSString *serverURL = [[compData objectAtIndex:row] valueForKey:@"url"];
    
    [[NSUserDefaults standardUserDefaults] setObject:serverURL forKey:@"ServerURL"];
    
    if ([appType isEqualToString:@"Familia"]) {
        [self performSegueWithIdentifier:@"ShowMainViewFam" sender:self];
    }
    else {
        [self performSegueWithIdentifier:@"ShowMainViewDet" sender:self];
    }
}

- (void) touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
    [[self view] endEditing:YES];
}

- (BOOL)textFieldShouldReturn:(UITextField *)textField{
    
    if (textField.tag == 0) {
        
        [txtSenha becomeFirstResponder];
    }
    else {
        [textField resignFirstResponder];
    }
    return YES;
}

- (void)textFieldDidBeginEditing:(UITextField *)textField {
    CGSize iOSDeviceScreenSize = [[UIScreen mainScreen] bounds].size;
    //-------------------IPHONE 4/4s/iPod--------------------
    if(iOSDeviceScreenSize.height == 480){
        CGPoint scrollPoint = CGPointMake(0, textField.frame.origin.y);
        [scrollView setContentOffset:scrollPoint animated:YES];
    }
}

- (void)textFieldDidEndEditing:(UITextField *)textField {
    CGSize iOSDeviceScreenSize = [[UIScreen mainScreen] bounds].size;
    //-------------------IPHONE 4/4s/iPod-------------------
    if(iOSDeviceScreenSize.height == 480)
        [scrollView setContentOffset:CGPointZero animated:YES];
}

- (NSString* )performLogin{
    
    NSString *loginURL = [[NSUserDefaults standardUserDefaults] stringForKey:@"LoginURL"];
    
    if (loginURL == nil || [loginURL  isEqual: @""])
        loginURL = @"http:comercial.bonyplus.com.br/scripts/cgiip.exe/WService=pdwebtst/mlapp/service/";
    
    //loginURL = @"http://mla.moderna.com.br/scripts/cgiip.exe/WService=MLA-MOD/service";
    //loginURL = @"http://szsolucoes.no-ip.biz:8089/cgi-bin/cgiip.exe/WService=pdweb/service";
    
    [[NSUserDefaults standardUserDefaults] setObject:loginURL forKey:@"ServerURL"];
    
    NSString *token = [[NSUserDefaults standardUserDefaults] stringForKey:@"DeviceToken"];
    
    NSString *pass = [txtSenha.text stringByReplacingOccurrencesOfString:@"=" withString:@"#igual#"];
    
    pass = [pass stringByReplacingOccurrencesOfString:@"&" withString:@"#ecomerc#"];
    
    NSString *strUrl = [NSString stringWithFormat:@"%@/doLogin.p?username=%@&password=%@&token=%@",loginURL,txtUsuario.text,pass,token];
    
    NSURL* url = [NSURL URLWithString:[strUrl stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding]];
    
    NSError* error = nil;
    
    UIApplication* app = [UIApplication sharedApplication];
    app.networkActivityIndicatorVisible = YES;
    
    
    NSData* jsonData = [NSURLConnection sendSynchronousRequest:[NSURLRequest requestWithURL:url cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:30] returningResponse:nil error:&error];
    
    app.networkActivityIndicatorVisible = NO;
    
    if (error) {
        return error.description;
    }
    
    
    NSString *success = [[NSJSONSerialization JSONObjectWithData:jsonData
                                                         options:NSJSONReadingMutableContainers error:&error] objectForKey:@"success"];
    
    NSString *appType = [[NSJSONSerialization JSONObjectWithData:jsonData
                                                         options:NSJSONReadingMutableContainers error:&error] objectForKey:@"appType"];
    
    
    if (appType != nil) {
        [[NSUserDefaults standardUserDefaults] setObject:appType forKey:@"appType"];
    }
    
    NSString *message = @"OK";
    if (![success isEqualToString: @"true"]) {
        message = [[NSJSONSerialization JSONObjectWithData:jsonData
                                                   options:NSJSONReadingMutableContainers error:&error] objectForKey:@"message"];
        if (message == nil)
            message = @"Não foi possível conectar ao servidor";
    }
    
    company = [[NSJSONSerialization JSONObjectWithData:jsonData
                                               options:NSJSONReadingMutableContainers error:&error] objectForKey:@"company"];
    
    
    return message;
}

// The number of columns of data
- (NSInteger)numberOfComponentsInPickerView:(UIPickerView *)pickerComp
{
    return 1;
}

// The number of rows of data
- (NSInteger)pickerView:(UIPickerView *)pickerComp numberOfRowsInComponent:(NSInteger)component
{
    
    return [pickerData count];
}

// The data to return for the row and component (column) that's being passed in
- (NSString*)pickerView:(UIPickerView *)pickerComp titleForRow:(NSInteger)row forComponent:(NSInteger)component
{
    return [pickerData objectAtIndex:row];
    
}

@end

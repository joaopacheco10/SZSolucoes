//
//  ApprovalViewController.m
//  MLApp
//
//  Created by SZ Solucoes on 25/04/14.
//  Copyright (c) 2014 SZ Solucoes. All rights reserved.
//

#import "ApprovalViewController.h"
#import "MBProgressHUD.h"
#import "DocTypeViewController.h"

@interface ApprovalViewController () <UIAlertViewDelegate>

@end

@implementation ApprovalViewController
@synthesize btAprovRej;
@synthesize txtRemarks;
@synthesize nrTrans;

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    // Do any additional setup after loading the view.
    
    if (self.acao == AcaoAprovar) {
        self.title = @"Aprovar";
        [btAprovRej setTitle:@"Aprovar" forState:UIControlStateNormal];
    }
    else {
        self.title = @"Rejeitar";
        [btAprovRej setTitle:@"Rejeitar" forState:UIControlStateNormal];
        //[btAprovRej setTitleColor:[UIColor redColor] forState:UIControlStateNormal] ;
        
    }
    
    //To make the border look very close to a UITextField
    [txtRemarks.layer setBorderColor:[[[UIColor grayColor] colorWithAlphaComponent:0.5] CGColor]];
    [txtRemarks.layer setBorderWidth:2.0];
    
    //The rounded corner part, where you specify your view's corner radius:
    txtRemarks.layer.cornerRadius = 5;
    txtRemarks.clipsToBounds = YES;
    
    self.automaticallyAdjustsScrollViewInsets = NO;
    
    //[txtRemarks setSelectedRange:NSMakeRange(0, 0)];
    
    
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

/*
 #pragma mark - Navigation
 
 // In a storyboard-based application, you will often want to do a little preparation before navigation
 - (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
 {
 // Get the new view controller using [segue destinationViewController].
 // Pass the selected object to the new view controller.
 }
 */

/*- (void) touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
 [[self view] endEditing:YES];
 }*/

- (IBAction)btAprovRejTap:(id)sender {
    MBProgressHUD *HUD = [MBProgressHUD showHUDAddedTo:self.view animated:YES];
    [self.view addSubview:HUD];
    //HUD.dimBackground = YES;
    //HUD.delegate = self;
    //HUD.color = [UIColor colorWithRed:(218/255.0) green:(83/255.0) blue:(32/255.0) alpha:1];
    HUD.labelText = @"Processando...";
    
    HUD.removeFromSuperViewOnHide = YES;
    
    dispatch_time_t popTime = dispatch_time(DISPATCH_TIME_NOW, 0.01 * NSEC_PER_SEC);
    dispatch_after(popTime, dispatch_get_main_queue(), ^(void){
        NSString *result = [self performApproval:btAprovRej.titleLabel.text];
        if ([result isEqualToString:@"OK"]) {
            
            NSString *aprovRej = self.acao == AcaoAprovar ? @"Aprovada" : @"Rejeitada";
            
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"MLApp"
                                                            message:[NSString stringWithFormat:@"Pendência %@ com sucesso!",aprovRej]
                                                           delegate:self
                                                  cancelButtonTitle:@"OK"
                                                  otherButtonTitles: nil];
            alert.tag = 1;
            [alert show];
            
            //[MBProgressHUD hideHUDForView:self.view animated:YES];
        }
        else {
            [HUD hide:YES];
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Erro"
                                                            message:[NSString stringWithFormat:@"Erro: %@",result ]
                                                           delegate:self
                                                  cancelButtonTitle:@"OK"
                                                  otherButtonTitles: nil];
            [alert show];
            return ;
            
        }
    });
}

- (NSString* )performApproval:(NSString*) btTappedTitle {
    
    NSString *serverURL = [[NSUserDefaults standardUserDefaults] stringForKey:@"ServerURL"];
    
    if (serverURL == nil || [serverURL  isEqual: @""])
        serverURL = @"http://comercial.bonyplus.com.br/scripts/cgiip.exe/WService=pdwebtst/mlapp/service";
    
    //serverURL = @"http://szsolucoes.no-ip.biz:8089/cgi-bin/cgiip.exe/WService=pdweb/service";
    
    NSString *userName = [[NSUserDefaults standardUserDefaults] stringForKey:@"UserName"];
    NSString *password = [[NSUserDefaults standardUserDefaults] stringForKey:@"Password"];
    NSString *appType  = [[NSUserDefaults standardUserDefaults] stringForKey:@"appType"];
    
    if ([self.docType isEqualToString:@"Acordo"]) {
        appType = @"Acordo";
    }
    
    NSString *remark = [txtRemarks.text stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
    
    
    NSString *url = [NSString stringWithFormat:@"%@/doApproval.p?username=%@&password=%@&action=%@&nrTrans=%@&remarks=%@&appType=%@",serverURL,userName,password,btTappedTitle,nrTrans,remark,appType];
    
    //NSString *url = [NSString stringWithFormat:@"%@/doApproval.p?username=fabricio&password=%@&action=%@&nrTrans=%@&remarks=%@&appType=%@",serverURL,password,btTappedTitle,nrTrans,remark,appType];
    
    NSError* error = nil;
    
    
    NSLog(@"URL - %@", url);
    
    UIApplication* app = [UIApplication sharedApplication];
    app.networkActivityIndicatorVisible = YES;
    
    
    //Faz a requisiçao ao servidor
    NSData *jsonData = [NSData dataWithContentsOfURL: [NSURL URLWithString:url] options:NSDataReadingUncached error:&error] ;
    app.networkActivityIndicatorVisible = NO;
    if (error) {
        return error.description;
    }
    
    //NSString *strData = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
    //NSLog(@"json: %@",strData );
    
    NSString *success = [[NSJSONSerialization JSONObjectWithData:jsonData
                                                         options:NSJSONReadingMutableContainers error:&error] objectForKey:@"success"];
    
    NSString *message = @"OK";
    if (![success isEqualToString: @"true"]) {
        message = [[NSJSONSerialization JSONObjectWithData:jsonData
                                                   options:NSJSONReadingMutableContainers error:&error] objectForKey:@"message"];
        if (message == nil)
            message = @"Não foi possível conectar ao servidor";
    }
    
    return message;
}

- (void) alertView:(UIAlertView *)alertView clickedButtonAtIndex:(NSInteger)buttonIndex {
    if (alertView.tag == 1) {
        DocTypeViewController *rootView = [self.navigationController.viewControllers objectAtIndex:0];
        [rootView doDocumentsRequest ];
        
        [self.navigationController popToRootViewControllerAnimated:YES];
    }
}

@end

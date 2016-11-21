//
//  DocTypeViewController.m
//  MLApp
//
//  Created by SZ Solucoes on 08/10/13.
//  Copyright (c) 2013 SZ Solucoes. All rights reserved.
//

#import "DocTypeViewController.h"
#import "DocumentsViewController.h"
#import "LoginViewController.h"
#import "MLAppAppDelegate.h"

@interface DocTypeViewController ()

@end

@implementation DocTypeViewController
@synthesize btSair;
NSArray *listOfDocs;
NSArray *listOfDocNumbers;
NSArray *listOfAgrees;
NSUInteger idxAgree;


- (id)initWithStyle:(UITableViewStyle)style
{
    self = [super initWithStyle:style];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    UINavigationBar *bar;
    bar = self.navigationController.navigationBar;
    bar.barTintColor = [UIColor orangeColor];
    //bar.barTintColor = [UIColor ]
    bar.translucent = YES;
    
    /*UIImage *toolBarImage = [UIImage imageNamed:@"logo80px.png"];
     
     [[UIToolbar appearance] setBackgroundImage:toolBarImage forToolbarPosition:UIToolbarPositionBottom barMetrics:UIBarMetricsDefault];
     [[UIToolbar appearance] setBackgroundImage:toolBarImage forToolbarPosition:UIToolbarPositionBottom barMetrics:UIBarMetricsLandscapePhone];*/
    
    btSair.title = @"Sair";
    
    
    
    [self.refreshControl addTarget:self
                            action:@selector(refreshView:)
                  forControlEvents:UIControlEventValueChanged];
    
    self.title = @"Documentos";
    
    UIBarButtonItem *backItem = [[UIBarButtonItem alloc] initWithTitle:@"Voltar"
                                                                 style:UIBarButtonItemStyleBordered
                                                                target:nil
                                                                action:nil];
    
    [self.navigationItem setBackBarButtonItem:backItem];
    
    MBProgressHUD *HUD = [MBProgressHUD showHUDAddedTo:self.view animated:YES];
    [self.view addSubview:HUD];
    //HUD.dimBackground = YES;
    //HUD.delegate = self;
    //HUD.color = [UIColor colorWithRed:(218/255.0) green:(83/255.0) blue:(32/255.0) alpha:1];
    HUD.labelText = @"Atualizando...";
    
    HUD.removeFromSuperViewOnHide = YES;
    
    dispatch_time_t popTime = dispatch_time(DISPATCH_TIME_NOW, 0.01 * NSEC_PER_SEC);
    dispatch_after(popTime, dispatch_get_main_queue(), ^(void){
        [self doDocumentsRequest];
        [HUD hide:YES];
        
    });
}
-(void)doDocumentsRequest
{
    
    idxAgree = 0;
    
    NSString *serverURL = [[NSUserDefaults standardUserDefaults] stringForKey:@"ServerURL"];
    
    if (serverURL == nil || [serverURL  isEqual: @""])
        serverURL = @"http://comercial.bonyplus.com.br/scripts/cgiip.exe/WService=pdwebtst/mlapp/service";
    
    //serverURL = @"http://szsolucoes.no-ip.biz:8089/cgi-bin/cgiip.exe/WService=pdweb/service";
    
    NSString *userName = [[NSUserDefaults standardUserDefaults] stringForKey:@"UserName"];
    
    NSString *appType = [[NSUserDefaults standardUserDefaults] stringForKey:@"appType"];
    
    NSString *url;
    NSString *urlAgree;
    
    if ([appType isEqualToString:@"Familia"]) {
        url = [NSString stringWithFormat:@"%@/getFamily.p?username=%@",serverURL,userName];
        urlAgree = [NSString stringWithFormat:@"%@/getAgreement.p?username=%@", serverURL, userName];
        //url = [NSString stringWithFormat:@"%@/getFamily.p?username=fabricio",serverURL];
        //urlAgree = [NSString stringWithFormat:@"%@/getAgreement.p?username=fabricio", serverURL];
    }
    
    if ([appType isEqualToString:@"Detalhe"]) {
        url = [NSString stringWithFormat:@"%@/getDocuments.p?username=%@",serverURL,userName];
    }
    
    NSError* error = nil;
    
    UIApplication* app = [UIApplication sharedApplication];
    app.networkActivityIndicatorVisible = YES;
    
    //Faz a requisiçao ao servidor
    NSData *jsonData = [NSData dataWithContentsOfURL: [NSURL URLWithString:url] options:NSDataReadingUncached error:&error];
    if (error) {
        UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Erro" message:[NSString stringWithFormat:@"Erro ao conectar ao servidor: %@",error.description ] delegate:self cancelButtonTitle:@"OK" otherButtonTitles: nil];
        [alert show];
        return ;
    }
    
    NSDictionary *resultados = [[NSJSONSerialization JSONObjectWithData:jsonData
                                                                options:NSJSONReadingMutableContainers error:&error] objectForKey:@"pendings"];
    
    error = nil;
    
    NSDictionary *agreeResult;
    
    if ([urlAgree length] != 0) {
        NSData *jsonAgree = [NSData dataWithContentsOfURL:[NSURL URLWithString:urlAgree] options:NSDataReadingUncached error:&error];
        if (error) {
            UIAlertView *alert = [[UIAlertView alloc] initWithTitle:@"Erro" message:[NSString stringWithFormat:@"Erro ao conectar ao servidor: %@",error.description ] delegate:self cancelButtonTitle:@"OK" otherButtonTitles: nil];
            [alert show];
            return ;
        }
        
        //NSString *strData = [[NSString alloc] initWithData:jsonAgree encoding:NSUTF8StringEncoding];
        //NSLog(@"json: %@",strData );
        
        agreeResult = [[NSJSONSerialization JSONObjectWithData:jsonAgree options:NSJSONReadingMutableContainers error:&error] objectForKey:@"pendings"];
    }
    
    
    //NSString *strData = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
    //NSLog(@"json: %@",strData );
    
    /*UIApplication* app = [UIApplication sharedApplication];
     app.networkActivityIndicatorVisible = YES;
     
     NSString *filePath = [[NSBundle mainBundle] pathForResource:@"documents" ofType:@"json"];
     
     NSData *jsonData = [NSData dataWithContentsOfFile:filePath];
     
     NSString *strData = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
     NSLog(@"json: %@",strData );
     
     NSError* error = nil;*/
    
    
    int pendencias = 0;
    
    if (!resultados) {
        NSLog(@"Erro ao ler JSON: %@", error);
    }
    else {
        
        NSArray *types = [resultados valueForKey:@"type"];
        NSArray *index = [resultados valueForKey:@"index"];
        listOfDocs = [resultados valueForKey:@"documents"];
        
        for (NSUInteger i = 0, count = [listOfDocs count]; i < count; i++) {
            NSArray *type = [listOfDocs objectAtIndex:i];
            pendencias += [[type valueForKey:@"documents"] count];
        }
        
        listOfDocNumbers = [resultados valueForKey:@"docNumber"];
        
        if (agreeResult) {
            
            NSArray *agreeDoc = [agreeResult valueForKey:@"type"];
            NSArray *idxAgree = [agreeResult valueForKey:@"index"];
            
            types = [types arrayByAddingObjectsFromArray:agreeDoc];
            index = [index arrayByAddingObjectsFromArray:idxAgree];
            
            listOfAgrees = [agreeResult valueForKey:@"agreements"];
            
            for (NSUInteger i = 0, count = [listOfAgrees count]; i < count; i++) {
                NSArray *type = [listOfAgrees objectAtIndex:i];
                pendencias += [[type valueForKey:@"agreements"] count];
            }
            
            NSArray *docNum = [agreeResult valueForKey:@"docNumber"];
            
            listOfDocNumbers = [listOfDocNumbers arrayByAddingObjectsFromArray:docNum];
            
        }
        
        NSLog(@"idx - %@", index);
        
        self.idxType = index;
        self.docTypes = types;
        
        [self.tableView reloadData];
    }
    
    app.networkActivityIndicatorVisible = NO;
    
    [app setApplicationIconBadgeNumber:pendencias];
    
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

#pragma mark - Table view data source

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView
{
    // Return the number of sections.
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section
{
    // Return the number of rows in the section.
    return [self.docTypes count];
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:  (NSIndexPath *)indexPath
{
    
    static NSString *CellIdentifier = @"Cell";
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier forIndexPath:indexPath];
    
    //NSDictionary *docs = [self.docTypes objectAtIndex:indexPath.row];
    
    NSArray *docs;
    NSArray *agrees;
    
    if ([listOfDocs count] > 0 && indexPath.row < [listOfDocs count]) {
        docs = [listOfDocs objectAtIndex:indexPath.row];
    } else {
        if ([listOfAgrees count] > 0) {
            agrees = [listOfAgrees objectAtIndex:idxAgree];
            idxAgree++;
        }
    }
    
    cell.textLabel.text =  [self.docTypes objectAtIndex: indexPath.row];
    
    if ([docs count] > 0) {
        cell.detailTextLabel.text = [NSString stringWithFormat:@"%lu",(unsigned long)[docs count]];
    } else {
        if ([agrees count] > 0) {
            
            cell.detailTextLabel.text = [NSString stringWithFormat:@"%lu",(unsigned long)[agrees count]];
            
        }
    }
    
    NSString *docNumber = [NSString stringWithFormat:@"%@",[listOfDocNumbers objectAtIndex:indexPath.row]];
    
    cell.imageView.image = [UIImage imageNamed:[NSString stringWithFormat:@"%@.png",docNumber]];
    
    return cell;
}

#pragma mark - Navigation

// In a story board-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    
    if ([[segue identifier] isEqualToString:@"ShowDocuments"]) {
        
        NSIndexPath *selectedRowIndex = [self.tableView indexPathForSelectedRow];
        
        if ([[self.docTypes objectAtIndex:selectedRowIndex.row] hasPrefix:@"Acordo"]) {
            
            int idx = [[self.idxType objectAtIndex:selectedRowIndex.row] intValue];
            
            DocumentsViewController *documentsViewController = [segue destinationViewController];
            documentsViewController.listOfAgrees = [listOfAgrees objectAtIndex:idx];
            documentsViewController.docType = [self.docTypes objectAtIndex:selectedRowIndex.row];
            
        } else {
            DocumentsViewController *documentsViewController = [segue destinationViewController];
            documentsViewController.listOfDocs = [listOfDocs objectAtIndex:selectedRowIndex.row];
            documentsViewController.docType = [self.docTypes objectAtIndex:selectedRowIndex.row];
        }
    }
}

- (void)refreshView:(UIRefreshControl *)sender {
    [sender beginRefreshing];
    [self doDocumentsRequest];
    [sender endRefreshing];
}

- (IBAction)btSairTap:(id)sender {
    
    MLAppAppDelegate * app = [[UIApplication sharedApplication] delegate];
    [app resetWindowToInitialView];
    //LoginViewController * controller = (LoginViewController *)[self.storyboard instantiateViewControllerWithIdentifier:@"LoginViewController"];
    //[self presentViewController:controller animated:YES completion:nil];
}
@end

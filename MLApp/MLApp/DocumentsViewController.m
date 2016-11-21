//
//  DocumentsViewController.m
//  MLApp
//
//  Created by SZ Solucoes on 30/10/13.
//  Copyright (c) 2013 SZ Solucoes. All rights reserved.
//

#import "DocumentsViewController.h"
#import "DocumentTableCell.h"
#import "AgreeTableViewCell.h"
#import "FamilyViewController.h"
#import "DetailsViewController.h"
#import "AgreeViewController.h"

@interface DocumentsViewController ()

@end

@implementation DocumentsViewController
@synthesize listOfDocs;
@synthesize listOfAgrees;
@synthesize docType;

- (id)initWithStyle:(UITableViewStyle)style
{
    self = [super initWithStyle:style];
    if (self) {
        
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    self.title =  docType;
    
    [self.tableView reloadData];
    
    UIBarButtonItem *backItem;
    
    if ([docType isEqualToString:@"Acordo Comercial"]) {
        
        backItem = [[UIBarButtonItem alloc] initWithTitle:@"Acordos"
                                                    style:UIBarButtonItemStyleBordered
                                                   target:nil
                                                   action:nil];
        
        
        
    } else {
        
        backItem = [[UIBarButtonItem alloc] initWithTitle:@"Pedidos"
                                                    style:UIBarButtonItemStyleBordered
                                                   target:nil
                                                   action:nil];
        
    }
    
    
    
    [self.navigationItem setBackBarButtonItem:backItem];
    
    // Uncomment the following line to preserve selection between presentations.
    // self.clearsSelectionOnViewWillAppear = NO;
    
    // Uncomment the following line to display an Edit button in the navigation bar for this view controller.
    // self.navigationItem.rightBarButtonItem = self.editButtonItem;
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
    if ([docType hasPrefix:@"Acordo"]) {
        return [self.listOfAgrees count];
    } else {
        return [self.listOfDocs count];
    }
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath
{
    
    if ([docType hasPrefix:@"Acordo"]) {
        
        static NSString *CellIdentifier = @"CellAgree";
        
        AgreeTableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier forIndexPath:indexPath];
        
        NSDictionary *agree = [self.listOfAgrees objectAtIndex:indexPath.row];
        
        cell.lblAgree.text  = [NSString stringWithFormat:@"%@", [agree valueForKey:@"numAgree"]];
        cell.lblClient.text = [agree valueForKey:@"customer"];
        cell.lblRepres.text = [agree valueForKey:@"user"];
        cell.lblValue.text  = [NSString stringWithFormat:@"R$ %@", [agree valueForKey:@"valueAgree"]];
        cell.lblAction.text = [agree valueForKey:@"action"];
        cell.lblDtPay.text  = [agree valueForKey:@"payDate"];
        cell.lblArea.text   = [agree valueForKey:@"area"];
        
        return cell;
        
    } else {
        
        static NSString *CellIdentifier = @"Cell";
        
        DocumentTableCell *cell = [tableView dequeueReusableCellWithIdentifier:CellIdentifier forIndexPath:indexPath];
        
        NSDictionary *doc;
        
        doc = [self.listOfDocs objectAtIndex:indexPath.row];
        
        cell.documentoLabel.text = [NSString stringWithFormat:@"%@", [doc valueForKey:@"docKey"]];
        cell.valorLabel.text = [NSString stringWithFormat:@"%@ %@",@"R$ ", [doc valueForKey:@"value"]];
        cell.dataLabel.text = [doc valueForKey:@"docDate"];
        cell.origemLabel.text = [doc valueForKey:@"user"];
        cell.fornecLabel.text = [doc valueForKey:@"supplier"];
        cell.obsLabel.text = [doc valueForKey:@"obsPedido"];
        
        return cell;
        
    }
    
}

/*
 // Override to support conditional editing of the table view.
 - (BOOL)tableView:(UITableView *)tableView canEditRowAtIndexPath:(NSIndexPath *)indexPath
 {
 // Return NO if you do not want the specified item to be editable.uuuuuu
 return YES;
 }
 */

/*
 // Override to support editing the table view.
 - (void)tableView:(UITableView *)tableView commitEditingStyle:(UITableViewCellEditingStyle)editingStyle forRowAtIndexPath:(NSIndexPath *)indexPath
 {
 if (editingStyle == UITableViewCellEditingStyleDelete) {
 // Delete the row from the data source
 [tableView deleteRowsAtIndexPaths:@[indexPath] withRowAnimation:UITableViewRowAnimationFade];
 }
 else if (editingStyle == UITableViewCellEditingStyleInsert) {
 // Create a new instance of the appropriate class, insert it into the array, and add a new row to the table view
 }
 }
 */

/*
 // Override to support rearranging the table view.
 - (void)tableView:(UITableView *)tableView moveRowAtIndexPath:(NSIndexPath *)fromIndexPath toIndexPath:(NSIndexPath *)toIndexPath
 {
 }
 */

/*
 // Override to support conditional rearranging of the table view.
 - (BOOL)tableView:(UITableView *)tableView canMoveRowAtIndexPath:(NSIndexPath *)indexPath
 {
 // Return NO if you do not want the item to be re-orderable.
 return YES;
 }
 */

- (void)tableView:(UITableView *)tableView didSelectRowAtIndexPath:(NSIndexPath *)indexPath {
    
    NSLog(@"SEGUE 1");
    
    NSString *appType = [[NSUserDefaults standardUserDefaults] stringForKey:@"appType"];
    
    if ([appType  isEqualToString:@"Detalhe"]) {
        
        [self performSegueWithIdentifier:@"ShowDetails" sender:nil];
        
    }
    
}

// In a story board-based application, you will often want to do a little preparation before navigation
- (void)prepareForSegue:(UIStoryboardSegue *)segue sender:(id)sender
{
    
    NSLog(@"%@", [segue identifier]);
    
    NSString *appType = [[NSUserDefaults standardUserDefaults] stringForKey:@"appType"];
    
    NSIndexPath *selectedRowIndex = [self.tableView indexPathForSelectedRow];
    
    NSDictionary *document = [self.listOfDocs objectAtIndex:selectedRowIndex.row];
    
    NSLog(@"Teste 1");
    
    if ([appType isEqualToString:@"Familia"] && [[segue identifier] isEqualToString:@"ShowFamily"]) {
        
        FamilyViewController *family = [segue destinationViewController];
        family.listFamily = [document objectForKey:@"family"];
        family.docKey = [document objectForKey:@"docKey"];
        family.doc = [self.listOfDocs objectAtIndex:selectedRowIndex.row];
        
    }
    
    if ([appType isEqualToString:@"Detalhe"] && [[segue identifier] isEqualToString:@"ShowDetails"]) {
        
        DetailsViewController *details = [segue destinationViewController];
        
        details.doc = [self.listOfDocs objectAtIndex:selectedRowIndex.row];
        details.family = [document objectForKey:@"family"];
        
    }
    
    if ([[segue identifier] isEqualToString:@"ShowAgree"]) {
        
        NSLog(@"Teste 2");
        
        AgreeViewController *agrees = [segue destinationViewController];
        
        agrees.agree = [self.listOfAgrees objectAtIndex:selectedRowIndex.row];
        
        NSLog(@"Teste 3");
        
        
    }
}




@end

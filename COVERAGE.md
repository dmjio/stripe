Charge tests
========
  - Charges a customer succesfully
  - Retrieves a charge succesfully
  - Updates a charge succesfully
  - Retrieves an expanded charge succesfully
  - Retrieves all charges
  - Captures a charge - 2 Step Payment Flow

Refund Tests
========
  - Creates a refund succesfully
  - Retrieves a refund succesfully
  - Retrieves a refund succesfully with expansion
  - Updates a refund succesfully
  - Retrieves all refunds for a Charge
  - Retrieves all refunds for a Charge with expansion

Customer tests
========
  - Creates an empty customer
  - Deletes a customer
  - Gets a customer
  - Gets a customer expandable
  - Gets customers
  - Gets customers expandable
  - Updates a customer

Card tests
========
  - Can create a Customer Card by CardNumber
  - Can create a Customer Card by TokenId
  - Can retrieve a Customer Card
  - Can retrieve a Customer's Card with expansion
  - Can retrieve a Customer's Cards
  - Can retrieve a Customer's Cards with Expansion
  - Can delete a Customer's Cards
  - Can update a Customer's Card

Recipient Card tests
========
  - Can create a RecipientCard by CardNumber
  - Can create a RecipientCard by Card TokenId
  - Fails to create a RecipientCard by BankAccount TokenId
  - Can retrieve a RecipientCard
  - Can retrieve a RecipientCard Expanded
  - Can retrieve a Recipient's Cards
  - Can retrieve a Recipient's Cards Expanded
  - Can delete a Recipient Card Expanded
  - Can update a Recipient's Card
  - Fails to add a Credit Card to a Recipient

Subscription tests
========
  - Succesfully creates a Subscription
  - Succesfully retrieves a Subscription
  - Succesfully retrieves a Subscription expanded
  - Succesfully retrieves a Customer's Subscriptions expanded
  - Succesfully retrieves a Customer's Subscriptions
  - Succesfully updates a Customer's Subscriptions
  - Succesfully cancels a Customer's Subscription

Plan tests
========
  - Succesfully creates a Plan
  - Succesfully creates a Plan and subscribes a customer with a TrialPeriod
  - Succesfully deletes a Plan
  - Succesfully updates a Plan
  - Succesfully retrieves a Plan
  - Succesfully retrieves a list of Plans

Coupon tests
========
  - Succesfully create a coupon
  - Succesfully retrieve a coupon
  - Succesfully delete a coupon
  - Succesfully update a coupon
  - Succesfully retrieves all coupons

Discount tests
========
  - Succesfully deletes a discount from a Customer
  - Succesfully deletes a discount from a Subscription

Invoice tests
========
  - Create an invoice and customer from a line item
  - Retrieve an Invoice
  - Retrieve an Invoice Expanded
  - Retrieve an Invoice's Line Items
  - Retrieve Invoices
  - Retrieve Invoices Expandable
  - Updates an Invoice
  - Retrieve an Upcoming Invoice
  - Pay an Invoice

Invoice item tests
========
  - Succesfully creates an invoice item
  - Succesfully retrieves an existing invoice item
  - Succesfully retrieves an existing invoice item expandable
  - Succesfully retrieves invoice items
  - Succesfully retrieves invoice items with expansion
  - Succesfully updates an existing invoice item
  - Succesfully deletes an invoice item

Dispute Tests
========
  - Creates a Dispute
  - Makes Dispute Under Review
  - Wins a Dispute
  - Loses a Dispute
  - Closes a Dispute

Transfer tests
========
  - Create a new transfer
  - Retrieves a transfer
  - Retrieves a transfer expandable
  - Retrieves transfers
  - Retrieves transfers expandable
  - Updates a transfer
  - Can't Cancel a committed transfer

Recipient tests
========
  - Succesfully creates an Individual Recipient
  - Succesfully creates a Corporation Recipient
  - Succesfully retrieves a Recipient
  - Succesfully retrieves a Recipient
  - Succesfully updates a Recipient
  - Succesfully deletes a Recipient

Application Fee tests
========
  - Succesfully fails to retrieve an unknown application fee
  - Succesfully retrieves all application fees

Application Fee Refund tests
========
  - Succesfully fails to refund an unknown application fee
  - Succesfully fails to retrieve a Application ID with an unknown Refund id
  - Succesfully fails to update an ApplicationRefund for an Invalid Fee ID
  - Succesfully fails to retrieve all ApplicationRefunds for an Invalid Fee ID

Account tests
========
  - Succesfully retrieves account information

Balance tests
========
  - Succesfully retrieves a Balance
  - Succesfully retrieves a Balance Transaction
  - Succesfully retrieves an Expanded Balance Transaction
  - Succesfully retrieves Balance Transaction History

Token tests
========
  - Can create a Card Token
  - Can create a Bank Account Token
  - Can retrieve an Existing Card Token
  - Can retrieve an Existing Bank Account Token

Event tests
========
  - Succesfully retrieves events

Finished in 195.9336 seconds
106 examples, 0 failures
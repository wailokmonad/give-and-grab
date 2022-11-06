# give-and-grab

A basic vesting smart contract in Plutus. There are two endpoints:

### give
Anyone can put some ADA (cannot be greater than 8 ADA) in the contract.

### grab
Anyone can grab all the ADA from the contract. However, the function will calculate the total amount of ADA and only allows taking 50% amount of ADA each time. Of course, you can grab as many times as you want.

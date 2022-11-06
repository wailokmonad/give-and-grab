# give-and-grab

A basic vesting smart contract in Plutus. There are two endpoints:

### give
Anyone can put some ADA (cannot be greater than 8 ADA) in the contract.

### grab
Anyone can grab all the ADA from the contract. However, the function will calculate the total amount of ADA locked in the contract and only allows taking 50% of them each time. Of course, you can grab as many times as you want.

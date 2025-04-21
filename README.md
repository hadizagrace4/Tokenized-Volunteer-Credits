# Tokenized Volunteer Credits

A Clarity smart contract that allows tracking and rewarding verified volunteer hours on the Stacks blockchain.

## Overview

This contract implements a system where:

1. Volunteers can register and log their volunteer activities
2. Organizations can register and verify volunteer activities
3. Verified volunteer hours are converted to credits
4. Credits can be transferred between volunteers

## Contract Functions

### Registration

- `register-volunteer`: Register as a volunteer
- `register-organization`: Register as an organization
- `verify-organization`: Admin can verify organizations

### Volunteer Activities

- `log-volunteer-activity`: Log hours volunteered at an organization
- `verify-activity`: Organizations verify volunteer activities
- `transfer-credits`: Transfer credits to another volunteer

### Admin Functions

- `set-admin`: Change the contract administrator
- `pause-contract`: Pause all contract operations
- `unpause-contract`: Resume contract operations

### Read-Only Functions

- `get-volunteer-info`: Get information about a volunteer
- `get-organization-info`: Get information about an organization
- `get-activity`: Get details of a specific activity
- `get-total-credits`: Get the total credits issued
- `get-admin`: Get the current admin address

## Usage Examples

### Register as a volunteer

```clarity
(contract-call? .credits register-volunteer "John Doe")
```

### Register as an organization

```clarity
(contract-call? .credits register-organization "Charity Inc" "We help people in need")
```

### Log volunteer activity

```clarity
(contract-call? .credits log-volunteer-activity 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM u5 "Helped at food bank" u1625097600)
```

### Verify volunteer activity

```clarity
(contract-call? .credits verify-activity u0)
```

### Transfer credits

```clarity
(contract-call? .credits transfer-credits 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM u3)
```

## Error Codes

- `ERR-NOT-AUTHORIZED (u100)`: Caller not authorized for this action
- `ERR-ALREADY-VERIFIED (u101)`: Activity already verified
- `ERR-NOT-FOUND (u102)`: Requested item not found
- `ERR-INVALID-HOURS (u103)`: Invalid number of hours
- `ERR-CONTRACT-PAUSED (u104)`: Contract is currently paused
- `ERR-ALREADY-REGISTERED (u105)`: Entity already registered
- `ERR-INSUFFICIENT-BALANCE (u106)`: Insufficient credit balance
- `ERR-ORGANIZATION-NOT-FOUND (u107)`: Organization not found
- `ERR-VOLUNTEER-NOT-FOUND (u108)`: Volunteer not found
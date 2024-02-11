# Yield Farming Unit Tests Documentation

This provides a detailed overview of the unit tests for the Yield Farming feature within the project. These tests are meticulously crafted to ensure the robustness and correctness of the yield farming functionalities, covering various scenarios from normal operations to error handling.

## Overview

The unit tests for Yield Farming are categorized under the "Yield Farming Unit Test" suite. This suite aims to validate the proper functioning of yield farming operations, including termination, harvesting, and adding rewards, under various conditions.

## Test Suite Details

### Tests Included

#### Termination of Yield Farming

- **Pass - Terminate Yield Farming**: Verifies that the termination of yield farming operations proceeds correctly under valid conditions.

- **Failure - Terminate Yield Farming - missing signature**: Tests the system's response to the absence of a required signature during termination.

- **Failure - Terminate Yield Farming - incorrect signature**: Ensures the system correctly rejects termination attempts with an incorrect signature.

#### Harvesting in Yield Farming

- **Pass - Harvest Yield Farming**: Confirms successful harvesting of rewards from yield farming under appropriate conditions.

- **Failure - Harvest Yield Farming - incorrect output datum**: Validates the system's ability to reject harvest operations with incorrect output data.

- **Failure - Harvest Yield Farming - miss owner's signature**: Tests the rejection of harvest operations missing the owner's signature.

- **Failure - Harvest Yield Farming - incorrect own index**: Ensures the system correctly handles harvest operations with an incorrect ownership index.

#### Adding Rewards to Yield Farming

- **Pass - Add Rewards Yield Farming**: Verifies the correct addition of rewards to the yield farming pool.

- **Failure - Add Rewards Yield Farming - incorrect output value**: Tests the system's response to incorrect output values during reward addition.

- **Failure - Add Rewards Yield Farming - incorrect auth index**: Validates the rejection of reward additions with an incorrect authorization index.

### Running the Tests

To execute the Yield Farming unit tests, follow the standard testing procedure as outlined in your project's documentation. Typically, this can be done using a command similar to:

```sh
cabal new-test --test-show-details=streaming
```

### Test Outcome Summary

In the latest test run:

All tests, including both successful operations and failure scenarios, passed successfully. This indicates that the Yield Farming feature correctly handles various operational and error conditions as expected.
The entire test suite was executed in approximately 0.06 seconds, demonstrating the efficiency of the Yield Farming feature's testing process.

```markdown
Test suite yield-farming-test: RUNNING...
Unit Test Group
  Yield Farming Unit Test
    Pass - Terminate Yield Farming:                               OK (0.05s)
    Failure - Terminate Yield Farming - missing signature:        OK
    Failure - Terminate Yield Farming - incorrect signature:      OK
    Pass - Harvest Yield Farming:                                 OK
    Failure - Harvest Yield Farming - incorrect output datum:     OK
    Failure - Harvest Yield Farming - miss owner's signature:     OK
    Failure - Harvest Yield Farming - incorrect own index:        OK
    Pass - Add Rewards Yield Farming:                             OK
    Failure - Add Rewards Yield Farming - incorrect output value: OK
    Failure - Add Rewards Yield Farming - incorrect auth index:   OK

All 10 tests passed (0.06s)
Test suite yield-farming-test: PASS
```

## Conclusion

The unit tests for the Yield Farming feature play a crucial role in ensuring the feature's reliability and stability. By covering a comprehensive range of scenarios, from successful operations to potential error cases, these tests help maintain the integrity and performance of the Yield Farming functionality within the project.

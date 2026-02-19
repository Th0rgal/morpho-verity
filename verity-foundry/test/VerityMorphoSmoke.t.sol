// SPDX-License-Identifier: GPL-2.0-or-later
pragma solidity ^0.8.19;

interface Vm {
    function readFile(string calldata path) external view returns (string memory);
    function parseBytes(string calldata) external pure returns (bytes memory);
    function prank(address caller) external;
    function warp(uint256) external;
}

interface IMorphoSubset {
    struct MarketParams {
        address loanToken;
        address collateralToken;
        address oracle;
        address irm;
        uint256 lltv;
    }

    function owner() external view returns (address);
    function feeRecipient() external view returns (address);
    function isIrmEnabled(address irm) external view returns (bool);
    function isLltvEnabled(uint256 lltv) external view returns (bool);
    function enableIrm(address irm) external;
    function enableLltv(uint256 lltv) external;
    function setFeeRecipient(address newFeeRecipient) external;
    function createMarket(MarketParams calldata marketParams) external;
    function lastUpdate(bytes32 id) external view returns (uint256);
    function market(bytes32 id)
        external
        view
        returns (
            uint256 totalSupplyAssets,
            uint256 totalSupplyShares,
            uint256 totalBorrowAssets,
            uint256 totalBorrowShares,
            uint256 lastUpdate_,
            uint256 fee
        );
    function position(bytes32 id, address user)
        external
        view
        returns (uint256 supplyShares, uint256 borrowShares, uint256 collateral);
}

contract VerityMorphoSmokeTest {
    Vm internal constant vm = Vm(address(uint160(uint256(keccak256("hevm cheat code")))));

    address internal constant OWNER = address(0xBEEF);
    IMorphoSubset internal morpho;

    function setUp() public {
        bytes memory initCode = bytes.concat(_loadBytecode("../compiler/yul/Morpho.bin"), abi.encode(OWNER));
        address deployed;
        assembly {
            deployed := create(0, add(initCode, 0x20), mload(initCode))
        }
        require(deployed != address(0), "deploy failed");
        morpho = IMorphoSubset(deployed);
    }

    function testOwnerInitialized() public view {
        require(morpho.owner() == OWNER, "owner mismatch");
        require(morpho.feeRecipient() == address(0), "feeRecipient mismatch");
    }

    function testOwnerCanEnableAndCreateMarket() public {
        address irm = address(0x1111);
        uint256 lltv = 0.8 ether;
        vm.prank(OWNER);
        morpho.enableIrm(irm);
        vm.prank(OWNER);
        morpho.enableLltv(lltv);
        require(morpho.isIrmEnabled(irm), "irm not enabled");
        require(morpho.isLltvEnabled(lltv), "lltv not enabled");

        address loanToken = address(0x2222);
        address collateralToken = address(0x3333);
        address oracle = address(0x4444);
        IMorphoSubset.MarketParams memory params =
            IMorphoSubset.MarketParams(loanToken, collateralToken, oracle, irm, lltv);
        bytes32 id = keccak256(abi.encode(loanToken, collateralToken, oracle, irm, lltv));

        vm.warp(1234567890);
        vm.prank(OWNER);
        morpho.createMarket(params);
        require(morpho.lastUpdate(id) == 1234567890, "lastUpdate mismatch");

        (
            ,
            ,
            ,
            ,
            uint256 marketLastUpdate,
            uint256 marketFee
        ) = morpho.market(id);
        require(marketLastUpdate == 1234567890, "market.lastUpdate mismatch");
        require(marketFee == 0, "market.fee mismatch");

        (uint256 supplyShares, uint256 borrowShares, uint256 collateral) = morpho.position(id, OWNER);
        require(supplyShares == 0, "position.supplyShares mismatch");
        require(borrowShares == 0, "position.borrowShares mismatch");
        require(collateral == 0, "position.collateral mismatch");
    }

    function testNonOwnerCannotEnableIrm() public {
        (bool ok,) = address(morpho).call(abi.encodeWithSignature("enableIrm(address)", address(0x1234)));
        require(!ok, "non-owner call should fail");
    }

    function _loadBytecode(string memory path) internal view returns (bytes memory) {
        bytes memory raw = bytes(vm.readFile(path));
        bytes memory trimmed = _trim(raw);
        bytes memory prefixed = bytes.concat(bytes("0x"), trimmed);
        return vm.parseBytes(string(prefixed));
    }

    function _trim(bytes memory input) internal pure returns (bytes memory) {
        uint256 start = 0;
        uint256 end = input.length;
        while (start < end && _isWhitespace(input[start])) start++;
        while (end > start && _isWhitespace(input[end - 1])) end--;
        bytes memory out = new bytes(end - start);
        for (uint256 i = 0; i < out.length; i++) out[i] = input[start + i];
        return out;
    }

    function _isWhitespace(bytes1 c) internal pure returns (bool) {
        return c == 0x20 || c == 0x0a || c == 0x0d || c == 0x09;
    }
}

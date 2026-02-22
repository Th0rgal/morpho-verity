// SPDX-License-Identifier: GPL-2.0-or-later
pragma solidity ^0.8.19;

interface Vm {
    struct Log {
        bytes32[] topics;
        bytes data;
        address emitter;
    }

    function readFile(string calldata path) external view returns (string memory);
    function parseBytes(string calldata) external pure returns (bytes memory);
    function prank(address caller) external;
    function warp(uint256) external;
    function recordLogs() external;
    function getRecordedLogs() external returns (Log[] memory);
    function expectEmit(bool checkTopic1, bool checkTopic2, bool checkTopic3, bool checkData, address emitter)
        external;
    function addr(uint256 privateKey) external returns (address);
    function sign(uint256 privateKey, bytes32 digest) external returns (uint8 v, bytes32 r, bytes32 s);
    function assume(bool condition) external;
    function label(address a, string calldata name) external;
}

interface IMorphoSubset {
    struct MarketParams {
        address loanToken;
        address collateralToken;
        address oracle;
        address irm;
        uint256 lltv;
    }
    struct Authorization {
        address authorizer;
        address authorized;
        bool isAuthorized;
        uint256 nonce;
        uint256 deadline;
    }
    struct Signature {
        uint8 v;
        bytes32 r;
        bytes32 s;
    }

    function owner() external view returns (address);
    function setOwner(address newOwner) external;
    function feeRecipient() external view returns (address);
    function isIrmEnabled(address irm) external view returns (bool);
    function isLltvEnabled(uint256 lltv) external view returns (bool);
    function isAuthorized(address authorizer, address authorized) external view returns (bool);
    function enableIrm(address irm) external;
    function enableLltv(uint256 lltv) external;
    function setFeeRecipient(address newFeeRecipient) external;
    function setAuthorization(address authorized, bool newIsAuthorized) external;
    function setAuthorizationWithSig(Authorization calldata authorization, Signature calldata signature) external;
    function createMarket(MarketParams calldata marketParams) external;
    function setFee(MarketParams calldata marketParams, uint256 newFee) external;
    function accrueInterest(MarketParams calldata marketParams) external;
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
    function idToMarketParams(bytes32 id)
        external
        view
        returns (address loanToken, address collateralToken, address oracle, address irm, uint256 lltv);
    function totalSupplyAssets(bytes32 id) external view returns (uint256);
    function totalSupplyShares(bytes32 id) external view returns (uint256);
    function position(bytes32 id, address user)
        external
        view
        returns (uint256 supplyShares, uint256 borrowShares, uint256 collateral);
    function extSloads(bytes32[] calldata slots) external view returns (bytes32[] memory);
    function supply(
        MarketParams calldata marketParams,
        uint256 assets,
        uint256 shares,
        address onBehalf,
        bytes calldata data
    ) external returns (uint256 assetsSupplied, uint256 sharesSupplied);
    function withdraw(
        MarketParams calldata marketParams,
        uint256 assets,
        uint256 shares,
        address onBehalf,
        address receiver
    ) external returns (uint256 assetsWithdrawn, uint256 sharesWithdrawn);
    function nonce(address authorizer) external view returns (uint256);
    function borrow(
        MarketParams calldata marketParams,
        uint256 assets,
        uint256 shares,
        address onBehalf,
        address receiver
    ) external returns (uint256 assetsBorrowed, uint256 sharesBorrowed);
    function repay(
        MarketParams calldata marketParams,
        uint256 assets,
        uint256 shares,
        address onBehalf,
        bytes calldata data
    ) external returns (uint256 assetsRepaid, uint256 sharesRepaid);
    function supplyCollateral(
        MarketParams calldata marketParams,
        uint256 assets,
        address onBehalf,
        bytes calldata data
    ) external;
    function withdrawCollateral(
        MarketParams calldata marketParams,
        uint256 assets,
        address onBehalf,
        address receiver
    ) external;
    function liquidate(
        MarketParams calldata marketParams,
        address borrower,
        uint256 seizedAssets,
        uint256 repaidShares,
        bytes calldata data
    ) external returns (uint256 seizedAssetsOut, uint256 repaidAssetsOut);
    function flashLoan(address token, uint256 assets, bytes calldata data) external;
    function totalBorrowAssets(bytes32 id) external view returns (uint256);
    function totalBorrowShares(bytes32 id) external view returns (uint256);
}

contract MockERC20 {
    string public name = "Mock";
    string public symbol = "MOCK";
    uint8 public decimals = 18;

    mapping(address => uint256) public balanceOf;
    mapping(address => mapping(address => uint256)) public allowance;
    event Transfer(address indexed from, address indexed to, uint256 amount);

    function mint(address to, uint256 amount) external {
        balanceOf[to] += amount;
        emit Transfer(address(0), to, amount);
    }

    function approve(address spender, uint256 amount) external returns (bool) {
        allowance[msg.sender][spender] = amount;
        return true;
    }

    function transferFrom(address from, address to, uint256 amount) external virtual returns (bool) {
        uint256 allowed = allowance[from][msg.sender];
        require(allowed >= amount, "allowance");
        uint256 bal = balanceOf[from];
        require(bal >= amount, "balance");
        allowance[from][msg.sender] = allowed - amount;
        balanceOf[from] = bal - amount;
        balanceOf[to] += amount;
        emit Transfer(from, to, amount);
        return true;
    }

    function transfer(address to, uint256 amount) external virtual returns (bool) {
        uint256 bal = balanceOf[msg.sender];
        require(bal >= amount, "balance");
        balanceOf[msg.sender] = bal - amount;
        balanceOf[to] += amount;
        emit Transfer(msg.sender, to, amount);
        return true;
    }
}

contract MockIRM {
    function borrowRate(
        IMorphoSubset.MarketParams calldata,
        uint256, uint256, uint256, uint256, uint256, uint256
    ) external pure returns (uint256) {
        return 0;
    }

    fallback() external {
        assembly {
            mstore(0, 0)
            return(0, 32)
        }
    }
}

contract MockERC20FalseTransferFrom is MockERC20 {
    function transferFrom(address, address, uint256) external pure override returns (bool) {
        return false;
    }
}

contract MockERC20FalseTransfer is MockERC20 {
    function transfer(address, uint256) external pure override returns (bool) {
        return false;
    }
}

contract MockOracle {
    uint256 public oraclePrice;

    constructor(uint256 _price) {
        oraclePrice = _price;
    }

    function setPrice(uint256 _price) external {
        oraclePrice = _price;
    }

    function price() external view returns (uint256) {
        return oraclePrice;
    }

    fallback() external {
        assembly {
            let p := sload(0)
            mstore(0, p)
            return(0, 32)
        }
    }
}

contract FlashBorrower {
    MockERC20 token;

    constructor(MockERC20 _token) {
        token = _token;
    }

    /// @dev Called by Morpho as onMorphoFlashLoan(uint256, bytes)
    fallback() external {
        // approve Morpho to pull the tokens back
        token.approve(msg.sender, type(uint256).max);
    }
}

contract VerityMorphoSmokeTest {
    Vm internal constant vm = Vm(address(uint160(uint256(keccak256("hevm cheat code")))));
    bytes32 internal constant DOMAIN_TYPEHASH =
        keccak256("EIP712Domain(uint256 chainId,address verifyingContract)");
    bytes32 internal constant AUTHORIZATION_TYPEHASH =
        keccak256("Authorization(address authorizer,address authorized,bool isAuthorized,uint256 nonce,uint256 deadline)");

    address internal constant OWNER = address(0xBEEF);
    IMorphoSubset internal morpho;

    event SetAuthorization(
        address indexed caller, address indexed authorizer, address indexed authorized, bool newIsAuthorized
    );
    event Supply(bytes32 indexed id, address caller, address indexed onBehalf, uint256 assets, uint256 shares);

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
            address storedLoanToken,
            address storedCollateralToken,
            address storedOracle,
            address storedIrm,
            uint256 storedLltv
        ) = morpho.idToMarketParams(id);
        require(storedLoanToken == loanToken, "idToMarketParams.loanToken mismatch");
        require(storedCollateralToken == collateralToken, "idToMarketParams.collateralToken mismatch");
        require(storedOracle == oracle, "idToMarketParams.oracle mismatch");
        require(storedIrm == irm, "idToMarketParams.irm mismatch");
        require(storedLltv == lltv, "idToMarketParams.lltv mismatch");

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

    function testCreateMarketWithHighBitAddresses() public {
        address irm = address(0xc7183455a4C133Ae270771860664b6B7ec320bB1);
        uint256 lltv = 0.8 ether;
        vm.prank(OWNER);
        morpho.enableIrm(irm);
        vm.prank(OWNER);
        morpho.enableLltv(lltv);

        address loanToken = address(0x2e234DAe75C793f67A35089C9d99245E1C58470b);
        address collateralToken = address(0xF62849F9A0B5Bf2913b396098F7c7019b51A820a);
        address oracle = address(0x5991A2dF15A8F6A256D3Ec51E99254Cd3fb576A9);
        IMorphoSubset.MarketParams memory params =
            IMorphoSubset.MarketParams(loanToken, collateralToken, oracle, irm, lltv);
        bytes32 id = keccak256(abi.encode(loanToken, collateralToken, oracle, irm, lltv));

        vm.warp(1700000000);
        vm.prank(OWNER);
        morpho.createMarket(params);
        require(morpho.lastUpdate(id) == 1700000000, "lastUpdate mismatch");
    }

    function testNonOwnerCannotEnableIrm() public {
        (bool ok,) = address(morpho).call(abi.encodeWithSignature("enableIrm(address)", address(0x1234)));
        require(!ok, "non-owner call should fail");
    }

    function testEnableLltvEventLayoutMatchesMorphoAbi() public {
        uint256 lltv = 0.8 ether;

        vm.recordLogs();
        vm.prank(OWNER);
        morpho.enableLltv(lltv);
        Vm.Log[] memory entries = vm.getRecordedLogs();

        bytes32 enableLltvSig = keccak256("EnableLltv(uint256)");
        bool found;
        for (uint256 i = 0; i < entries.length; ++i) {
            if (entries[i].emitter != address(morpho) || entries[i].topics.length == 0 || entries[i].topics[0] != enableLltvSig)
            {
                continue;
            }
            found = true;
            require(entries[i].topics.length == 1, "EnableLltv should have no indexed args");
            require(entries[i].data.length == 32, "EnableLltv should encode lltv in data");
            require(abi.decode(entries[i].data, (uint256)) == lltv, "EnableLltv data lltv mismatch");
            break;
        }
        require(found, "EnableLltv event missing");
    }

    function testSetAuthorizationEmitsEvent() public {
        address authorizer = address(0xA11CE);
        address authorized = address(0xB0B);

        vm.expectEmit(true, true, true, true, address(morpho));
        emit SetAuthorization(authorizer, authorizer, authorized, true);

        vm.prank(authorizer);
        morpho.setAuthorization(authorized, true);
    }

    function testSetAuthorizationWithSig() public {
        uint256 authorizerPk = 0xA11CE;
        address authorizer = vm.addr(authorizerPk);
        address authorized = address(0xB0B);
        uint256 deadline = block.timestamp + 1 days;

        IMorphoSubset.Authorization memory authorization = IMorphoSubset.Authorization({
            authorizer: authorizer,
            authorized: authorized,
            isAuthorized: true,
            nonce: morpho.nonce(authorizer),
            deadline: deadline
        });

        bytes32 structHash = keccak256(
            abi.encode(
                AUTHORIZATION_TYPEHASH,
                authorization.authorizer,
                authorization.authorized,
                authorization.isAuthorized,
                authorization.nonce,
                authorization.deadline
            )
        );
        bytes32 domainSeparator = keccak256(abi.encode(DOMAIN_TYPEHASH, block.chainid, address(morpho)));
        bytes32 digest = keccak256(abi.encodePacked("\x19\x01", domainSeparator, structHash));
        (uint8 v, bytes32 r, bytes32 s) = vm.sign(authorizerPk, digest);

        IMorphoSubset.Signature memory signature = IMorphoSubset.Signature({v: v, r: r, s: s});
        morpho.setAuthorizationWithSig(authorization, signature);

        require(morpho.nonce(authorizer) == 1, "nonce should increment");
        require(morpho.isAuthorized(authorizer, authorized), "authorization should be set");
    }

    function testSupplyUpdatesMarketAndPosition() public {
        MockERC20 loanToken = new MockERC20();
        address collateralToken = address(0x3333);
        address oracle = address(0x4444);
        address irm = address(0x1111);
        uint256 lltv = 0.8 ether;

        vm.prank(OWNER);
        morpho.enableIrm(irm);
        vm.prank(OWNER);
        morpho.enableLltv(lltv);

        IMorphoSubset.MarketParams memory params =
            IMorphoSubset.MarketParams(address(loanToken), collateralToken, oracle, irm, lltv);
        bytes32 id = keccak256(abi.encode(address(loanToken), collateralToken, oracle, irm, lltv));

        vm.warp(1234567890);
        vm.prank(OWNER);
        morpho.createMarket(params);

        address supplier = address(0xA11CE);
        loanToken.mint(supplier, 1_000 ether);
        vm.prank(supplier);
        loanToken.approve(address(morpho), type(uint256).max);

        vm.prank(supplier);
        (uint256 assetsSupplied, uint256 sharesSupplied) = morpho.supply(params, 100 ether, 0, supplier, "");
        require(assetsSupplied == 100 ether, "assetsSupplied mismatch");
        require(sharesSupplied > 0, "sharesSupplied should be positive");

        uint256 totalSupplyAssets = morpho.totalSupplyAssets(id);
        uint256 totalSupplyShares = morpho.totalSupplyShares(id);
        require(totalSupplyAssets == assetsSupplied, "totalSupplyAssets mismatch");
        require(totalSupplyShares == sharesSupplied, "totalSupplyShares mismatch");

        (
            uint256 marketTotalSupplyAssets,
            uint256 marketTotalSupplyShares,
            ,
            ,
            uint256 marketLastUpdate,
            uint256 marketFee
        ) = morpho.market(id);
        require(marketTotalSupplyAssets == assetsSupplied, "market.totalSupplyAssets mismatch");
        require(marketTotalSupplyShares == sharesSupplied, "market.totalSupplyShares mismatch");
        require(marketLastUpdate == 1234567890, "market.lastUpdate mismatch");
        require(marketFee == 0, "market.fee mismatch");

        (uint256 supplyShares,,) = morpho.position(id, supplier);
        require(supplyShares == sharesSupplied, "position.supplyShares mismatch");
    }

    function testSupplyRevertReasonOnFalseTransferFromReturn() public {
        MockERC20FalseTransferFrom loanToken = new MockERC20FalseTransferFrom();
        IMorphoSubset.MarketParams memory params =
            IMorphoSubset.MarketParams(address(loanToken), address(0x3333), address(0x4444), address(0x1111), 0.8 ether);

        vm.prank(OWNER);
        morpho.enableIrm(address(0x1111));
        vm.prank(OWNER);
        morpho.enableLltv(0.8 ether);
        vm.prank(OWNER);
        morpho.createMarket(params);

        address supplier = address(0xA11CE);
        loanToken.mint(supplier, 1_000 ether);
        vm.prank(supplier);
        loanToken.approve(address(morpho), type(uint256).max);

        vm.prank(supplier);
        (bool ok, bytes memory ret) = address(morpho).call(
            abi.encodeWithSelector(IMorphoSubset.supply.selector, params, 100 ether, 0, supplier, bytes(""))
        );
        require(!ok, "supply should revert");
        _assertDecodedError(ret, "transferFrom returned false", "supply revert reason mismatch");
    }

    function testExtSloadsMatchesGetters() public {
        MockERC20 loanToken = new MockERC20();
        address collateralToken = address(0x3333);
        address oracle = address(0x4444);
        address irm = address(0x1111);
        uint256 lltv = 0.8 ether;

        vm.prank(OWNER);
        morpho.enableIrm(irm);
        vm.prank(OWNER);
        morpho.enableLltv(lltv);

        IMorphoSubset.MarketParams memory params =
            IMorphoSubset.MarketParams(address(loanToken), collateralToken, oracle, irm, lltv);
        bytes32 id = keccak256(abi.encode(address(loanToken), collateralToken, oracle, irm, lltv));

        vm.warp(1234567890);
        vm.prank(OWNER);
        morpho.createMarket(params);

        address supplier = address(0xA11CE);
        loanToken.mint(supplier, 1_000 ether);
        vm.prank(supplier);
        loanToken.approve(address(morpho), type(uint256).max);
        vm.prank(supplier);
        (uint256 assetsSupplied, uint256 sharesSupplied) = morpho.supply(params, 100 ether, 0, supplier, "");

        bytes32[] memory slots = new bytes32[](2);
        slots[0] = _mappingSlot(3, id); // packed Market[id].{totalSupplyAssets,totalSupplyShares}
        slots[1] = _nestedMappingSlot(17, id, supplier); // positionSupplyShares[id][supplier]

        bytes32[] memory values = morpho.extSloads(slots);
        require(values.length == 2, "extSloads length mismatch");
        require(uint128(uint256(values[0])) == assetsSupplied, "extSloads assets mismatch");
        require(uint256(values[0]) >> 128 == sharesSupplied, "extSloads shares mismatch");
        require(uint256(values[1]) == sharesSupplied, "extSloads position mismatch");
    }

    function testAccrueInterestUpdatesLastUpdate() public {
        MockERC20 loanToken = new MockERC20();
        address collateralToken = address(0x3333);
        address oracle = address(0x4444);
        address irm = address(new MockIRM());
        uint256 lltv = 0.8 ether;

        vm.prank(OWNER);
        morpho.enableIrm(irm);
        vm.prank(OWNER);
        morpho.enableLltv(lltv);

        IMorphoSubset.MarketParams memory params =
            IMorphoSubset.MarketParams(address(loanToken), collateralToken, oracle, irm, lltv);
        bytes32 id = keccak256(abi.encode(address(loanToken), collateralToken, oracle, irm, lltv));

        vm.warp(1234567890);
        vm.prank(OWNER);
        morpho.createMarket(params);
        require(morpho.lastUpdate(id) == 1234567890, "lastUpdate after create mismatch");

        vm.warp(1234567900);
        morpho.accrueInterest(params);
        require(morpho.lastUpdate(id) == 1234567900, "lastUpdate after accrue mismatch");

        morpho.accrueInterest(params);
        require(morpho.lastUpdate(id) == 1234567900, "lastUpdate should be stable at same timestamp");
    }

    function testPackedMarketSlotTracksAccrueAndSetFee() public {
        MockERC20 loanToken = new MockERC20();
        address collateralToken = address(0x3333);
        address oracle = address(0x4444);
        address irm = address(new MockIRM());
        uint256 lltv = 0.8 ether;

        vm.prank(OWNER);
        morpho.enableIrm(irm);
        vm.prank(OWNER);
        morpho.enableLltv(lltv);

        IMorphoSubset.MarketParams memory params =
            IMorphoSubset.MarketParams(address(loanToken), collateralToken, oracle, irm, lltv);
        bytes32 id = keccak256(abi.encode(address(loanToken), collateralToken, oracle, irm, lltv));

        vm.warp(1234567890);
        vm.prank(OWNER);
        morpho.createMarket(params);

        bytes32[] memory slots = new bytes32[](1);
        slots[0] = bytes32(uint256(_mappingSlot(3, id)) + 2); // packed Market[ID].{lastUpdate,fee}

        bytes32[] memory packedAfterCreate = morpho.extSloads(slots);
        require(uint128(uint256(packedAfterCreate[0])) == 1234567890, "packed lastUpdate after create mismatch");
        require(uint256(packedAfterCreate[0]) >> 128 == 0, "packed fee after create mismatch");

        vm.warp(1234567900);
        morpho.accrueInterest(params);
        bytes32[] memory packedAfterAccrue = morpho.extSloads(slots);
        require(uint128(uint256(packedAfterAccrue[0])) == 1234567900, "packed lastUpdate after accrue mismatch");

        vm.prank(OWNER);
        morpho.setFee(params, 0.1 ether);
        bytes32[] memory packedAfterSetFee = morpho.extSloads(slots);
        require(uint128(uint256(packedAfterSetFee[0])) == 1234567900, "packed lastUpdate after setFee mismatch");
        require(uint256(packedAfterSetFee[0]) >> 128 == 0.1 ether, "packed fee after setFee mismatch");
    }

    function testSetFeeAccruesInterestBeforeUpdating() public {
        MockERC20 loanToken = new MockERC20();
        address collateralToken = address(0x3333);
        address oracle = address(0x4444);
        address irm = address(new MockIRM());
        uint256 lltv = 0.8 ether;

        vm.prank(OWNER);
        morpho.enableIrm(irm);
        vm.prank(OWNER);
        morpho.enableLltv(lltv);

        IMorphoSubset.MarketParams memory params =
            IMorphoSubset.MarketParams(address(loanToken), collateralToken, oracle, irm, lltv);
        bytes32 id = keccak256(abi.encode(address(loanToken), collateralToken, oracle, irm, lltv));

        vm.warp(1234567890);
        vm.prank(OWNER);
        morpho.createMarket(params);
        require(morpho.lastUpdate(id) == 1234567890, "lastUpdate after create mismatch");

        // Advance time — setFee should accrue interest first and update lastUpdate
        vm.warp(1234567900);
        vm.prank(OWNER);
        morpho.setFee(params, 0.1 ether);
        require(morpho.lastUpdate(id) == 1234567900, "lastUpdate should be updated by setFee accrual");
    }

    function testPackedMarketSlotTracksSupplyAndWithdraw() public {
        MockERC20 loanToken = new MockERC20();
        IMorphoSubset.MarketParams memory params =
            IMorphoSubset.MarketParams(address(loanToken), address(0x3333), address(0x4444), address(0x1111), 0.8 ether);
        bytes32 id = keccak256(abi.encode(address(loanToken), address(0x3333), address(0x4444), address(0x1111), 0.8 ether));

        vm.prank(OWNER);
        morpho.enableIrm(address(0x1111));
        vm.prank(OWNER);
        morpho.enableLltv(0.8 ether);

        vm.warp(1234567890);
        vm.prank(OWNER);
        morpho.createMarket(params);

        address supplier = address(0xA11CE);
        address receiver = address(0xC0FFEE);
        loanToken.mint(supplier, 1_000 ether);
        vm.prank(supplier);
        loanToken.approve(address(morpho), type(uint256).max);

        bytes32[] memory slots = new bytes32[](1);
        slots[0] = bytes32(uint256(_mappingSlot(3, id))); // packed Market[ID].{totalSupplyAssets,totalSupplyShares}

        vm.prank(supplier);
        morpho.supply(params, 100 ether, 0, supplier, "");

        bytes32[] memory packedAfterSupply = morpho.extSloads(slots);
        require(uint128(uint256(packedAfterSupply[0])) == morpho.totalSupplyAssets(id), "packed assets after supply mismatch");
        require(uint256(packedAfterSupply[0]) >> 128 == morpho.totalSupplyShares(id), "packed shares after supply mismatch");

        vm.prank(supplier);
        morpho.withdraw(params, 40 ether, 0, supplier, receiver);

        bytes32[] memory packedAfterWithdraw = morpho.extSloads(slots);
        require(
            uint128(uint256(packedAfterWithdraw[0])) == morpho.totalSupplyAssets(id),
            "packed assets after withdraw mismatch"
        );
        require(
            uint256(packedAfterWithdraw[0]) >> 128 == morpho.totalSupplyShares(id),
            "packed shares after withdraw mismatch"
        );
    }

    function testWithdrawUpdatesMarketAndPosition() public {
        MockERC20 loanToken = new MockERC20();
        IMorphoSubset.MarketParams memory params =
            IMorphoSubset.MarketParams(address(loanToken), address(0x3333), address(0x4444), address(0x1111), 0.8 ether);
        bytes32 id = keccak256(abi.encode(address(loanToken), address(0x3333), address(0x4444), address(0x1111), 0.8 ether));

        vm.prank(OWNER);
        morpho.enableIrm(address(0x1111));
        vm.prank(OWNER);
        morpho.enableLltv(0.8 ether);

        vm.warp(1234567890);
        vm.prank(OWNER);
        morpho.createMarket(params);

        address supplier = address(0xA11CE);
        address receiver = address(0xC0FFEE);
        loanToken.mint(supplier, 1_000 ether);
        vm.prank(supplier);
        loanToken.approve(address(morpho), type(uint256).max);

        vm.prank(supplier);
        morpho.supply(params, 100 ether, 0, supplier, "");
        uint256 sharesBeforeWithdraw = morpho.totalSupplyShares(id);
        require(sharesBeforeWithdraw > 0, "sharesBeforeWithdraw should be positive");

        vm.prank(supplier);
        (uint256 assetsWithdrawn, uint256 sharesWithdrawn) = morpho.withdraw(params, 40 ether, 0, supplier, receiver);
        require(assetsWithdrawn == 40 ether, "assetsWithdrawn mismatch");
        require(sharesWithdrawn > 0, "sharesWithdrawn should be positive");

        uint256 totalSupplyAssets = morpho.totalSupplyAssets(id);
        uint256 totalSupplyShares = morpho.totalSupplyShares(id);
        require(totalSupplyAssets == 60 ether, "totalSupplyAssets after withdraw mismatch");
        require(totalSupplyShares == sharesBeforeWithdraw - sharesWithdrawn, "totalSupplyShares after withdraw mismatch");

        (uint256 supplyShares,,) = morpho.position(id, supplier);
        require(supplyShares == sharesBeforeWithdraw - sharesWithdrawn, "position.supplyShares after withdraw mismatch");
        require(loanToken.balanceOf(receiver) == assetsWithdrawn, "receiver token balance mismatch");
    }

    function testWithdrawRevertReasonOnFalseTransferReturn() public {
        MockERC20FalseTransfer loanToken = new MockERC20FalseTransfer();
        IMorphoSubset.MarketParams memory params =
            IMorphoSubset.MarketParams(address(loanToken), address(0x3333), address(0x4444), address(0x1111), 0.8 ether);

        vm.prank(OWNER);
        morpho.enableIrm(address(0x1111));
        vm.prank(OWNER);
        morpho.enableLltv(0.8 ether);
        vm.prank(OWNER);
        morpho.createMarket(params);

        address supplier = address(0xA11CE);
        loanToken.mint(supplier, 1_000 ether);
        vm.prank(supplier);
        loanToken.approve(address(morpho), type(uint256).max);

        vm.prank(supplier);
        morpho.supply(params, 100 ether, 0, supplier, "");

        vm.prank(supplier);
        (bool ok, bytes memory ret) = address(morpho).call(
            abi.encodeWithSelector(IMorphoSubset.withdraw.selector, params, 10 ether, 0, supplier, address(0xC0FFEE))
        );
        require(!ok, "withdraw should revert");
        _assertDecodedError(ret, "transfer returned false", "withdraw revert reason mismatch");
    }

    function testWithdrawEmitsEventBeforeTokenTransfer() public {
        MockERC20 loanToken = new MockERC20();
        IMorphoSubset.MarketParams memory params =
            IMorphoSubset.MarketParams(address(loanToken), address(0x3333), address(0x4444), address(0x1111), 0.8 ether);

        vm.prank(OWNER);
        morpho.enableIrm(address(0x1111));
        vm.prank(OWNER);
        morpho.enableLltv(0.8 ether);

        vm.warp(1234567890);
        vm.prank(OWNER);
        morpho.createMarket(params);

        address supplier = address(0xA11CE);
        address receiver = address(0xC0FFEE);
        loanToken.mint(supplier, 1_000 ether);
        vm.prank(supplier);
        loanToken.approve(address(morpho), type(uint256).max);

        vm.prank(supplier);
        morpho.supply(params, 100 ether, 0, supplier, "");

        vm.recordLogs();
        vm.prank(supplier);
        morpho.withdraw(params, 40 ether, 0, supplier, receiver);
        Vm.Log[] memory entries = vm.getRecordedLogs();

        bytes32 withdrawSig = keccak256("Withdraw(bytes32,address,address,address,uint256,uint256)");
        bytes32 transferSig = keccak256("Transfer(address,address,uint256)");
        uint256 withdrawIndex = type(uint256).max;
        uint256 transferIndex = type(uint256).max;
        for (uint256 i = 0; i < entries.length; ++i) {
            if (entries[i].topics.length == 0) continue;
            if (entries[i].topics[0] == withdrawSig && entries[i].emitter == address(morpho) && withdrawIndex == type(uint256).max)
            {
                withdrawIndex = i;
            }
            if (entries[i].topics[0] == transferSig && entries[i].emitter == address(loanToken) && transferIndex == type(uint256).max)
            {
                transferIndex = i;
            }
        }
        require(withdrawIndex != type(uint256).max, "withdraw event missing");
        require(transferIndex != type(uint256).max, "transfer event missing");
        require(withdrawIndex < transferIndex, "withdraw event emitted after token transfer");
    }

    function testSupplyEventLayoutMatchesMorphoAbi() public {
        MockERC20 loanToken = new MockERC20();
        IMorphoSubset.MarketParams memory params =
            IMorphoSubset.MarketParams(address(loanToken), address(0x3333), address(0x4444), address(0x1111), 0.8 ether);
        bytes32 id = keccak256(abi.encode(params.loanToken, params.collateralToken, params.oracle, params.irm, params.lltv));

        vm.prank(OWNER);
        morpho.enableIrm(address(0x1111));
        vm.prank(OWNER);
        morpho.enableLltv(0.8 ether);

        vm.warp(1234567890);
        vm.prank(OWNER);
        morpho.createMarket(params);

        address supplier = address(0xA11CE);
        loanToken.mint(supplier, 1_000 ether);
        vm.prank(supplier);
        loanToken.approve(address(morpho), type(uint256).max);

        vm.recordLogs();
        vm.prank(supplier);
        (uint256 assetsSupplied, uint256 sharesSupplied) = morpho.supply(params, 100 ether, 0, supplier, "");
        Vm.Log[] memory entries = vm.getRecordedLogs();

        bytes32 supplySig = keccak256("Supply(bytes32,address,address,uint256,uint256)");
        bool found;
        for (uint256 i = 0; i < entries.length; ++i) {
            if (entries[i].emitter != address(morpho) || entries[i].topics.length == 0 || entries[i].topics[0] != supplySig) {
                continue;
            }
            found = true;
            require(entries[i].topics.length == 3, "supply event should have 2 indexed args");
            require(entries[i].topics[1] == id, "supply topic id mismatch");
            require(entries[i].topics[2] == bytes32(uint256(uint160(supplier))), "supply topic onBehalf mismatch");
            require(entries[i].data.length == 96, "supply event data length mismatch");
            (address caller_, uint256 assets_, uint256 shares_) = abi.decode(entries[i].data, (address, uint256, uint256));
            require(caller_ == supplier, "supply caller data mismatch");
            require(assets_ == assetsSupplied, "supply assets data mismatch");
            require(shares_ == sharesSupplied, "supply shares data mismatch");
            break;
        }
        require(found, "supply event missing");
    }

    function testWithdrawRejectsZeroReceiver() public {
        MockERC20 loanToken = new MockERC20();
        IMorphoSubset.MarketParams memory params =
            IMorphoSubset.MarketParams(address(loanToken), address(0x3333), address(0x4444), address(0x1111), 0.8 ether);

        vm.prank(OWNER);
        morpho.enableIrm(address(0x1111));
        vm.prank(OWNER);
        morpho.enableLltv(0.8 ether);

        vm.warp(1234567890);
        vm.prank(OWNER);
        morpho.createMarket(params);

        address supplier = address(0xA11CE);
        loanToken.mint(supplier, 1_000 ether);
        vm.prank(supplier);
        loanToken.approve(address(morpho), type(uint256).max);

        vm.prank(supplier);
        morpho.supply(params, 100 ether, 0, supplier, "");

        vm.prank(supplier);
        (bool ok,) = address(morpho).call(
            abi.encodeWithSelector(IMorphoSubset.withdraw.selector, params, 1 ether, 0, supplier, address(0))
        );
        require(!ok, "withdraw to zero receiver should fail");
    }

    // ── helpers to set up a market with collateral+borrow support ──

    function _createMarketWithOracle()
        internal
        returns (
            MockERC20 loanToken,
            MockERC20 collateralToken,
            MockOracle oracle,
            IMorphoSubset.MarketParams memory params,
            bytes32 id
        )
    {
        loanToken = new MockERC20();
        collateralToken = new MockERC20();
        // Oracle price = 1e36 means 1 collateral = 1 loanToken
        oracle = new MockOracle(1e36);
        address irm = address(new MockIRM());
        uint256 lltv = 0.8 ether;

        vm.prank(OWNER);
        morpho.enableIrm(irm);
        vm.prank(OWNER);
        morpho.enableLltv(lltv);

        params = IMorphoSubset.MarketParams(address(loanToken), address(collateralToken), address(oracle), irm, lltv);
        id = keccak256(abi.encode(address(loanToken), address(collateralToken), address(oracle), irm, lltv));

        vm.warp(1234567890);
        vm.prank(OWNER);
        morpho.createMarket(params);
    }

    // ── supplyCollateral ──

    function testSupplyCollateralUpdatesPosition() public {
        (MockERC20 loanToken, MockERC20 collateralToken,, IMorphoSubset.MarketParams memory params, bytes32 id)
            = _createMarketWithOracle();

        address user = address(0xA11CE);
        collateralToken.mint(user, 1_000 ether);
        vm.prank(user);
        collateralToken.approve(address(morpho), type(uint256).max);

        vm.prank(user);
        morpho.supplyCollateral(params, 500 ether, user, "");

        (, , uint256 collateral) = morpho.position(id, user);
        require(collateral == 500 ether, "collateral mismatch");
        require(collateralToken.balanceOf(address(morpho)) == 500 ether, "morpho collateral balance mismatch");
    }

    // ── borrow + repay ──

    function testBorrowAndRepay() public {
        (MockERC20 loanToken, MockERC20 collateralToken,, IMorphoSubset.MarketParams memory params, bytes32 id)
            = _createMarketWithOracle();

        // 1. Supplier provides liquidity
        address supplier = address(0xA11CE);
        loanToken.mint(supplier, 10_000 ether);
        vm.prank(supplier);
        loanToken.approve(address(morpho), type(uint256).max);
        vm.prank(supplier);
        morpho.supply(params, 5_000 ether, 0, supplier, "");

        // 2. Borrower posts collateral
        address borrower = address(0xB0B);
        collateralToken.mint(borrower, 10_000 ether);
        vm.prank(borrower);
        collateralToken.approve(address(morpho), type(uint256).max);
        vm.prank(borrower);
        morpho.supplyCollateral(params, 2_000 ether, borrower, "");

        // 3. Borrow
        vm.prank(borrower);
        (uint256 assetsBorrowed, uint256 sharesBorrowed) = morpho.borrow(params, 100 ether, 0, borrower, borrower);
        require(assetsBorrowed == 100 ether, "assetsBorrowed mismatch");
        require(sharesBorrowed > 0, "sharesBorrowed should be positive");
        require(loanToken.balanceOf(borrower) == 100 ether, "borrower should receive loan tokens");

        uint256 totalBorrow = morpho.totalBorrowAssets(id);
        require(totalBorrow == 100 ether, "totalBorrowAssets mismatch");

        (, uint256 posBorrowShares,) = morpho.position(id, borrower);
        require(posBorrowShares == sharesBorrowed, "position.borrowShares mismatch");

        // 4. Repay
        vm.prank(borrower);
        loanToken.approve(address(morpho), type(uint256).max);
        vm.prank(borrower);
        (uint256 assetsRepaid, uint256 sharesRepaid) = morpho.repay(params, 100 ether, 0, borrower, "");
        require(assetsRepaid == 100 ether, "assetsRepaid mismatch");
        require(sharesRepaid == sharesBorrowed, "sharesRepaid mismatch");

        uint256 totalBorrowAfter = morpho.totalBorrowAssets(id);
        require(totalBorrowAfter == 0, "totalBorrowAssets after repay should be 0");

        (, uint256 posBorrowSharesAfter,) = morpho.position(id, borrower);
        require(posBorrowSharesAfter == 0, "position.borrowShares after repay should be 0");
    }

    // ── withdrawCollateral ──

    function testWithdrawCollateral() public {
        (MockERC20 loanToken, MockERC20 collateralToken,, IMorphoSubset.MarketParams memory params, bytes32 id)
            = _createMarketWithOracle();

        address user = address(0xA11CE);
        collateralToken.mint(user, 1_000 ether);
        vm.prank(user);
        collateralToken.approve(address(morpho), type(uint256).max);
        vm.prank(user);
        morpho.supplyCollateral(params, 500 ether, user, "");

        address receiver = address(0xC0FFEE);
        vm.prank(user);
        morpho.withdrawCollateral(params, 200 ether, user, receiver);

        (, , uint256 collateral) = morpho.position(id, user);
        require(collateral == 300 ether, "collateral after withdraw mismatch");
        require(collateralToken.balanceOf(receiver) == 200 ether, "receiver collateral balance mismatch");
    }

    // ── liquidate ──

    function testLiquidateUnhealthyPosition() public {
        (MockERC20 loanToken, MockERC20 collateralToken, MockOracle oracle, IMorphoSubset.MarketParams memory params, bytes32 id)
            = _createMarketWithOracle();

        // 1. Supplier provides liquidity
        address supplier = address(0xA11CE);
        loanToken.mint(supplier, 100_000 ether);
        vm.prank(supplier);
        loanToken.approve(address(morpho), type(uint256).max);
        vm.prank(supplier);
        morpho.supply(params, 50_000 ether, 0, supplier, "");

        // 2. Borrower posts collateral and borrows
        address borrower = address(0xB0B);
        collateralToken.mint(borrower, 1_000 ether);
        vm.prank(borrower);
        collateralToken.approve(address(morpho), type(uint256).max);
        vm.prank(borrower);
        morpho.supplyCollateral(params, 1_000 ether, borrower, "");

        // Borrow 500 (safe with 1000 collateral at 0.8 LLTV = max 800)
        vm.prank(borrower);
        morpho.borrow(params, 500 ether, 0, borrower, borrower);

        // 3. Drop oracle price to make position unhealthy
        // At price=0.5e36, collateral worth = 1000 * 0.5 = 500, max borrow = 500 * 0.8 = 400
        // But borrower has 500 borrowed → unhealthy
        oracle.setPrice(0.5e36);

        // 4. Liquidator seizes collateral
        address liquidator = address(0xDEAD);
        loanToken.mint(liquidator, 100_000 ether);
        vm.prank(liquidator);
        loanToken.approve(address(morpho), type(uint256).max);

        vm.prank(liquidator);
        (uint256 seizedAssets, uint256 repaidAssets) = morpho.liquidate(params, borrower, 10 ether, 0, "");
        require(seizedAssets == 10 ether, "seizedAssets mismatch");
        require(repaidAssets > 0, "repaidAssets should be positive");

        // Verify collateral was seized
        (, , uint256 collateralAfter) = morpho.position(id, borrower);
        require(collateralAfter == 990 ether, "collateral after liquidation mismatch");
        require(collateralToken.balanceOf(liquidator) == 10 ether, "liquidator should receive collateral");
    }

    // ── flashLoan ──

    function testFlashLoan() public {
        MockERC20 token = new MockERC20();
        // Seed Morpho with tokens (flashLoan transfers from Morpho's balance)
        token.mint(address(morpho), 1_000 ether);

        FlashBorrower borrower = new FlashBorrower(token);
        // Give the borrower tokens to repay (flashLoan transfers back same amount)
        token.mint(address(borrower), 1_000 ether);

        vm.prank(address(borrower));
        morpho.flashLoan(address(token), 100 ether, "");

        // After flashLoan: Morpho balance should be restored
        require(token.balanceOf(address(morpho)) == 1_000 ether, "morpho balance should be unchanged after flashLoan");
    }

    function testFlashLoanRejectsZeroAssets() public {
        MockERC20 token = new MockERC20();
        vm.prank(address(0xA11CE));
        (bool ok,) = address(morpho).call(
            abi.encodeWithSelector(IMorphoSubset.flashLoan.selector, address(token), 0, bytes(""))
        );
        require(!ok, "flashLoan with zero assets should revert");
    }

    // ── borrow revert cases ──

    function testBorrowRejectsInsufficientCollateral() public {
        (MockERC20 loanToken, MockERC20 collateralToken,, IMorphoSubset.MarketParams memory params, bytes32 id)
            = _createMarketWithOracle();

        address supplier = address(0xA11CE);
        loanToken.mint(supplier, 10_000 ether);
        vm.prank(supplier);
        loanToken.approve(address(morpho), type(uint256).max);
        vm.prank(supplier);
        morpho.supply(params, 5_000 ether, 0, supplier, "");

        // Borrower posts small collateral, tries to borrow too much
        address borrower = address(0xB0B);
        collateralToken.mint(borrower, 100 ether);
        vm.prank(borrower);
        collateralToken.approve(address(morpho), type(uint256).max);
        vm.prank(borrower);
        morpho.supplyCollateral(params, 100 ether, borrower, "");

        // With 100 collateral, lltv=0.8, max borrow = 80 tokens. Try 90.
        vm.prank(borrower);
        (bool ok,) = address(morpho).call(
            abi.encodeWithSelector(IMorphoSubset.borrow.selector, params, 90 ether, 0, borrower, borrower)
        );
        require(!ok, "borrow exceeding health factor should revert");
    }

    // ── admin operations ──

    function testSetOwnerUpdatesOwner() public {
        address newOwner = address(0xCAFE);
        vm.prank(OWNER);
        morpho.setOwner(newOwner);
        require(morpho.owner() == newOwner, "owner should be updated");
    }

    function testSetOwnerRejectsNonOwner() public {
        (bool ok,) = address(morpho).call(abi.encodeWithSelector(IMorphoSubset.setOwner.selector, address(0xCAFE)));
        require(!ok, "non-owner setOwner should revert");
    }

    function testSetFeeRecipientUpdates() public {
        address recipient = address(0xFEE);
        vm.prank(OWNER);
        morpho.setFeeRecipient(recipient);
        require(morpho.feeRecipient() == recipient, "feeRecipient should be updated");
    }

    function testSetFeeRecipientRejectsNonOwner() public {
        (bool ok,) = address(morpho).call(
            abi.encodeWithSelector(IMorphoSubset.setFeeRecipient.selector, address(0xFEE))
        );
        require(!ok, "non-owner setFeeRecipient should revert");
    }

    // ── multi-market isolation ──

    function testMultiMarketIsolation() public {
        MockERC20 loanA = new MockERC20();
        MockERC20 loanB = new MockERC20();
        MockERC20 collateralToken = new MockERC20();
        MockOracle oracle = new MockOracle(1e36);
        address irm = address(new MockIRM());
        uint256 lltv = 0.8 ether;

        vm.prank(OWNER);
        morpho.enableIrm(irm);
        vm.prank(OWNER);
        morpho.enableLltv(lltv);

        IMorphoSubset.MarketParams memory paramsA =
            IMorphoSubset.MarketParams(address(loanA), address(collateralToken), address(oracle), irm, lltv);
        IMorphoSubset.MarketParams memory paramsB =
            IMorphoSubset.MarketParams(address(loanB), address(collateralToken), address(oracle), irm, lltv);
        bytes32 idA = keccak256(abi.encode(address(loanA), address(collateralToken), address(oracle), irm, lltv));
        bytes32 idB = keccak256(abi.encode(address(loanB), address(collateralToken), address(oracle), irm, lltv));

        vm.warp(1234567890);
        vm.prank(OWNER);
        morpho.createMarket(paramsA);
        vm.prank(OWNER);
        morpho.createMarket(paramsB);

        // Supply 100 to market A only
        address supplier = address(0xA11CE);
        loanA.mint(supplier, 1_000 ether);
        vm.prank(supplier);
        loanA.approve(address(morpho), type(uint256).max);
        vm.prank(supplier);
        morpho.supply(paramsA, 100 ether, 0, supplier, "");

        // Market A should have supply, market B should be empty
        require(morpho.totalSupplyAssets(idA) == 100 ether, "market A totalSupplyAssets mismatch");
        require(morpho.totalSupplyAssets(idB) == 0, "market B should have no supply");
        require(morpho.totalSupplyShares(idB) == 0, "market B should have no shares");

        (uint256 sharesA,,) = morpho.position(idA, supplier);
        (uint256 sharesB,,) = morpho.position(idB, supplier);
        require(sharesA > 0, "supplier should have shares in market A");
        require(sharesB == 0, "supplier should have no shares in market B");
    }

    // ── setAuthorizationWithSig edge cases ──

    function testSetAuthorizationWithSigRejectsReplay() public {
        uint256 authorizerPk = 0xA11CE;
        address authorizer = vm.addr(authorizerPk);
        address authorized = address(0xB0B);
        uint256 deadline = block.timestamp + 1 days;

        IMorphoSubset.Authorization memory authorization = IMorphoSubset.Authorization({
            authorizer: authorizer,
            authorized: authorized,
            isAuthorized: true,
            nonce: morpho.nonce(authorizer),
            deadline: deadline
        });

        bytes32 structHash = keccak256(
            abi.encode(AUTHORIZATION_TYPEHASH, authorizer, authorized, true, authorization.nonce, deadline)
        );
        bytes32 domainSeparator = keccak256(abi.encode(DOMAIN_TYPEHASH, block.chainid, address(morpho)));
        bytes32 digest = keccak256(abi.encodePacked("\x19\x01", domainSeparator, structHash));
        (uint8 v, bytes32 r, bytes32 s) = vm.sign(authorizerPk, digest);

        IMorphoSubset.Signature memory signature = IMorphoSubset.Signature({v: v, r: r, s: s});
        morpho.setAuthorizationWithSig(authorization, signature);
        require(morpho.nonce(authorizer) == 1, "nonce should increment");

        // Replay the same signature — nonce is now 1 but authorization.nonce is still 0
        (bool ok,) = address(morpho).call(
            abi.encodeWithSelector(
                IMorphoSubset.setAuthorizationWithSig.selector, authorization, signature
            )
        );
        require(!ok, "replay should revert (nonce already used)");
    }

    function testSetAuthorizationWithSigRejectsExpiredDeadline() public {
        uint256 authorizerPk = 0xA11CE;
        address authorizer = vm.addr(authorizerPk);
        address authorized = address(0xB0B);

        vm.warp(1000);
        uint256 deadline = 500; // already in the past

        IMorphoSubset.Authorization memory authorization = IMorphoSubset.Authorization({
            authorizer: authorizer,
            authorized: authorized,
            isAuthorized: true,
            nonce: morpho.nonce(authorizer),
            deadline: deadline
        });

        bytes32 structHash = keccak256(
            abi.encode(AUTHORIZATION_TYPEHASH, authorizer, authorized, true, authorization.nonce, deadline)
        );
        bytes32 domainSeparator = keccak256(abi.encode(DOMAIN_TYPEHASH, block.chainid, address(morpho)));
        bytes32 digest = keccak256(abi.encodePacked("\x19\x01", domainSeparator, structHash));
        (uint8 v, bytes32 r, bytes32 s) = vm.sign(authorizerPk, digest);

        IMorphoSubset.Signature memory signature = IMorphoSubset.Signature({v: v, r: r, s: s});
        (bool ok,) = address(morpho).call(
            abi.encodeWithSelector(
                IMorphoSubset.setAuthorizationWithSig.selector, authorization, signature
            )
        );
        require(!ok, "expired deadline should revert");
    }

    function testSetAuthorizationWithSigRejectsWrongSigner() public {
        uint256 wrongPk = 0xDEAD;
        uint256 authorizerPk = 0xA11CE;
        address authorizer = vm.addr(authorizerPk);
        address authorized = address(0xB0B);
        uint256 deadline = block.timestamp + 1 days;

        IMorphoSubset.Authorization memory authorization = IMorphoSubset.Authorization({
            authorizer: authorizer,
            authorized: authorized,
            isAuthorized: true,
            nonce: morpho.nonce(authorizer),
            deadline: deadline
        });

        bytes32 structHash = keccak256(
            abi.encode(AUTHORIZATION_TYPEHASH, authorizer, authorized, true, authorization.nonce, deadline)
        );
        bytes32 domainSeparator = keccak256(abi.encode(DOMAIN_TYPEHASH, block.chainid, address(morpho)));
        bytes32 digest = keccak256(abi.encodePacked("\x19\x01", domainSeparator, structHash));
        // Sign with wrong key
        (uint8 v, bytes32 r, bytes32 s) = vm.sign(wrongPk, digest);

        IMorphoSubset.Signature memory signature = IMorphoSubset.Signature({v: v, r: r, s: s});
        (bool ok,) = address(morpho).call(
            abi.encodeWithSelector(
                IMorphoSubset.setAuthorizationWithSig.selector, authorization, signature
            )
        );
        require(!ok, "wrong signer should revert");
    }

    // ── fuzz tests ──

    function testFuzzSupplyWithdrawRoundTrip(uint256 supplyAmount) public {
        vm.assume(supplyAmount > 0 && supplyAmount <= 1e30);

        MockERC20 loanToken = new MockERC20();
        IMorphoSubset.MarketParams memory params =
            IMorphoSubset.MarketParams(address(loanToken), address(0x3333), address(0x4444), address(0x1111), 0.8 ether);
        bytes32 id = keccak256(abi.encode(address(loanToken), address(0x3333), address(0x4444), address(0x1111), 0.8 ether));

        vm.prank(OWNER);
        morpho.enableIrm(address(0x1111));
        vm.prank(OWNER);
        morpho.enableLltv(0.8 ether);
        vm.warp(1234567890);
        vm.prank(OWNER);
        morpho.createMarket(params);

        address supplier = address(0xA11CE);
        loanToken.mint(supplier, supplyAmount);
        vm.prank(supplier);
        loanToken.approve(address(morpho), type(uint256).max);

        vm.prank(supplier);
        (uint256 assetsSupplied, uint256 sharesSupplied) = morpho.supply(params, supplyAmount, 0, supplier, "");
        require(assetsSupplied == supplyAmount, "supply amount mismatch");
        require(sharesSupplied > 0, "shares should be positive");

        // Withdraw everything
        vm.prank(supplier);
        (uint256 assetsWithdrawn,) = morpho.withdraw(params, 0, sharesSupplied, supplier, supplier);
        require(assetsWithdrawn == supplyAmount, "full withdraw should return all assets");
        require(morpho.totalSupplyAssets(id) == 0, "totalSupplyAssets should be 0 after full withdraw");
    }

    function testFuzzBorrowRepayRoundTrip(uint256 borrowAmount) public {
        vm.assume(borrowAmount > 0 && borrowAmount <= 1e27);

        (MockERC20 loanToken, MockERC20 collateralToken,, IMorphoSubset.MarketParams memory params, bytes32 id) =
            _createMarketWithOracle();

        // Supplier provides liquidity
        address supplier = address(0xA11CE);
        loanToken.mint(supplier, 1e30);
        vm.prank(supplier);
        loanToken.approve(address(morpho), type(uint256).max);
        vm.prank(supplier);
        morpho.supply(params, 1e30, 0, supplier, "");

        // Borrower posts ample collateral (10x borrowAmount to always be safe)
        address borrower = address(0xB0B);
        uint256 collateralAmount = borrowAmount * 10;
        collateralToken.mint(borrower, collateralAmount);
        vm.prank(borrower);
        collateralToken.approve(address(morpho), type(uint256).max);
        vm.prank(borrower);
        morpho.supplyCollateral(params, collateralAmount, borrower, "");

        // Borrow
        vm.prank(borrower);
        (uint256 assetsBorrowed, uint256 sharesBorrowed) = morpho.borrow(params, borrowAmount, 0, borrower, borrower);
        require(assetsBorrowed == borrowAmount, "borrow amount mismatch");
        require(sharesBorrowed > 0, "borrow shares should be positive");

        // Repay everything
        vm.prank(borrower);
        loanToken.approve(address(morpho), type(uint256).max);
        vm.prank(borrower);
        (uint256 assetsRepaid,) = morpho.repay(params, 0, sharesBorrowed, borrower, "");
        require(assetsRepaid == borrowAmount, "full repay should match borrow");
        require(morpho.totalBorrowAssets(id) == 0, "totalBorrowAssets should be 0 after full repay");
    }

    function testFuzzCollateralSupplyWithdraw(uint256 collateralAmount) public {
        vm.assume(collateralAmount > 0 && collateralAmount <= 1e30);

        (, MockERC20 collateralToken,, IMorphoSubset.MarketParams memory params, bytes32 id) =
            _createMarketWithOracle();

        address user = address(0xA11CE);
        collateralToken.mint(user, collateralAmount);
        vm.prank(user);
        collateralToken.approve(address(morpho), type(uint256).max);

        vm.prank(user);
        morpho.supplyCollateral(params, collateralAmount, user, "");

        (,, uint256 storedCollateral) = morpho.position(id, user);
        require(storedCollateral == collateralAmount, "collateral should match deposited amount");

        // Withdraw all
        vm.prank(user);
        morpho.withdrawCollateral(params, collateralAmount, user, user);

        (,, uint256 collateralAfter) = morpho.position(id, user);
        require(collateralAfter == 0, "collateral should be 0 after full withdraw");
        require(collateralToken.balanceOf(user) == collateralAmount, "user should get all collateral back");
    }

    function testFuzzFlashLoan(uint256 amount) public {
        vm.assume(amount > 0 && amount <= 1e30);

        MockERC20 token = new MockERC20();
        token.mint(address(morpho), amount);

        FlashBorrower borrower = new FlashBorrower(token);
        token.mint(address(borrower), amount);

        vm.prank(address(borrower));
        morpho.flashLoan(address(token), amount, "");

        require(token.balanceOf(address(morpho)) == amount, "morpho balance should be unchanged after flashLoan");
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

    function _decodeErrorString(bytes memory ret) internal pure returns (string memory) {
        if (ret.length < 68) return "";
        bytes4 selector;
        assembly {
            selector := mload(add(ret, 0x20))
        }
        if (selector != 0x08c379a0) return "";
        bytes memory payload = new bytes(ret.length - 4);
        for (uint256 i = 0; i < payload.length; ++i) {
            payload[i] = ret[i + 4];
        }
        return abi.decode(payload, (string));
    }

    function _assertDecodedError(bytes memory ret, string memory expected, string memory context) internal pure {
        require(keccak256(bytes(_decodeErrorString(ret))) == keccak256(bytes(expected)), context);
    }

    function _isWhitespace(bytes1 c) internal pure returns (bool) {
        return c == 0x20 || c == 0x0a || c == 0x0d || c == 0x09;
    }

    function _mappingSlot(uint256 slot, bytes32 key) internal pure returns (bytes32) {
        return keccak256(abi.encode(key, slot));
    }

    function _nestedMappingSlot(uint256 slot, bytes32 key1, address key2) internal pure returns (bytes32) {
        bytes32 outer = keccak256(abi.encode(key1, slot));
        return keccak256(abi.encode(key2, outer));
    }
}

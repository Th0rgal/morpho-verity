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

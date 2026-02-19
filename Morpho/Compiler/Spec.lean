import Compiler.ContractSpec

namespace Morpho.Compiler.Spec

open Compiler.ContractSpec

private def wad : Nat := 1000000000000000000
private def maxFee : Nat := 250000000000000000

private def requireOwner : Stmt :=
  Stmt.require (Expr.eq Expr.caller (Expr.storage "owner")) "not owner"

private def marketIdExpr (params : Array Expr) : Expr :=
  Expr.externalCall "keccakMarketParams" params.toList

/--
A best-effort Morpho ContractSpec target that compiles to Yul and preserves the
core owner/market-creation configuration behavior used in proof scaffolding.

Notes:
- ABI selectors are set manually to match IMorpho tuple signatures.
- Some IMorpho functions are intentionally omitted because current Verity
  ContractSpec does not yet support multi-value returns needed for full parity.
-/
def morphoSpec : ContractSpec := {
  name := "Morpho"
  fields := [
    { name := "owner", ty := .address },
    { name := "feeRecipient", ty := .address },
    { name := "isIrmEnabled", ty := .mappingTyped (.simple .address) },
    { name := "isLltvEnabled", ty := .mappingTyped (.simple .uint256) },
    { name := "isAuthorized", ty := .mappingTyped (.nested .address .address) },
    { name := "nonce", ty := .mappingTyped (.simple .address) },
    { name := "marketLastUpdate", ty := .mappingTyped (.simple .uint256) },
    { name := "marketFee", ty := .mappingTyped (.simple .uint256) },
    { name := "marketTotalSupplyAssets", ty := .mappingTyped (.simple .uint256) },
    { name := "marketTotalSupplyShares", ty := .mappingTyped (.simple .uint256) },
    { name := "marketTotalBorrowAssets", ty := .mappingTyped (.simple .uint256) },
    { name := "marketTotalBorrowShares", ty := .mappingTyped (.simple .uint256) },
    { name := "idToLoanToken", ty := .mappingTyped (.simple .uint256) },
    { name := "idToCollateralToken", ty := .mappingTyped (.simple .uint256) },
    { name := "idToOracle", ty := .mappingTyped (.simple .uint256) },
    { name := "idToIrm", ty := .mappingTyped (.simple .uint256) },
    { name := "idToLltv", ty := .mappingTyped (.simple .uint256) },
    { name := "positionSupplyShares", ty := .mappingTyped (.nested .uint256 .address) }
  ]
  constructor := some {
    params := [{ name := "initialOwner", ty := .address }]
    body := [
      Stmt.require
        (Expr.logicalNot (Expr.eq (Expr.constructorArg 0) (Expr.literal 0)))
        "zero address",
      Stmt.setStorage "owner" (Expr.constructorArg 0),
      Stmt.setStorage "feeRecipient" (Expr.literal 0)
    ]
  }
  externals := [
    { name := "keccakMarketParams"
      params := [.address, .address, .address, .address, .uint256]
      returnType := some .uint256
      axiomNames := ["market_id_deterministic"] }
  ]
  functions := [
    -- Views
    {
      name := "DOMAIN_SEPARATOR"
      params := []
      returnType := some .uint256
      body := [Stmt.return (Expr.literal 0)]
    },
    {
      name := "owner"
      params := []
      returnType := some .address
      body := [Stmt.return (Expr.storage "owner")]
    },
    {
      name := "feeRecipient"
      params := []
      returnType := some .address
      body := [Stmt.return (Expr.storage "feeRecipient")]
    },
    {
      name := "isIrmEnabled"
      params := [{ name := "irm", ty := .address }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mapping "isIrmEnabled" (Expr.param "irm"))]
    },
    {
      name := "isLltvEnabled"
      params := [{ name := "lltv", ty := .uint256 }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mappingUint "isLltvEnabled" (Expr.param "lltv"))]
    },
    {
      name := "isAuthorized"
      params := [
        { name := "authorizer", ty := .address },
        { name := "authorized", ty := .address }
      ]
      returnType := some .uint256
      body := [Stmt.return (Expr.mapping2 "isAuthorized" (Expr.param "authorizer") (Expr.param "authorized"))]
    },
    {
      name := "nonce"
      params := [{ name := "authorizer", ty := .address }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mapping "nonce" (Expr.param "authorizer"))]
    },
    {
      name := "lastUpdate"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mappingUint "marketLastUpdate" (Expr.param "id"))]
    },
    {
      name := "totalSupplyAssets"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mappingUint "marketTotalSupplyAssets" (Expr.param "id"))]
    },
    {
      name := "totalSupplyShares"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mappingUint "marketTotalSupplyShares" (Expr.param "id"))]
    },
    {
      name := "totalBorrowAssets"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mappingUint "marketTotalBorrowAssets" (Expr.param "id"))]
    },
    {
      name := "totalBorrowShares"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mappingUint "marketTotalBorrowShares" (Expr.param "id"))]
    },
    {
      name := "fee"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := some .uint256
      body := [Stmt.return (Expr.mappingUint "marketFee" (Expr.param "id"))]
    },
    {
      name := "idToMarketParams"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := none
      returns := [.address, .address, .address, .address, .uint256]
      body := [
        Stmt.letVar "loanToken" (Expr.mappingUint "idToLoanToken" (Expr.param "id")),
        Stmt.letVar "collateralToken" (Expr.mappingUint "idToCollateralToken" (Expr.param "id")),
        Stmt.letVar "oracle" (Expr.mappingUint "idToOracle" (Expr.param "id")),
        Stmt.letVar "irm" (Expr.mappingUint "idToIrm" (Expr.param "id")),
        Stmt.letVar "lltv" (Expr.mappingUint "idToLltv" (Expr.param "id")),
        Stmt.returnValues [
          Expr.localVar "loanToken",
          Expr.localVar "collateralToken",
          Expr.localVar "oracle",
          Expr.localVar "irm",
          Expr.localVar "lltv"
        ]
      ]
    },
    {
      name := "position"
      params := [
        { name := "id", ty := .bytes32 },
        { name := "user", ty := .address }
      ]
      returnType := none
      returns := [.uint256, .uint256, .uint256]
      body := [
        Stmt.returnValues [
          Expr.mapping2 "positionSupplyShares" (Expr.param "id") (Expr.param "user"),
          Expr.literal 0,
          Expr.literal 0
        ]
      ]
    },
    {
      name := "market"
      params := [{ name := "id", ty := .bytes32 }]
      returnType := none
      returns := [.uint256, .uint256, .uint256, .uint256, .uint256, .uint256]
      body := [
        Stmt.letVar "totalSupplyAssets" (Expr.mappingUint "marketTotalSupplyAssets" (Expr.param "id")),
        Stmt.letVar "totalSupplyShares" (Expr.mappingUint "marketTotalSupplyShares" (Expr.param "id")),
        Stmt.letVar "totalBorrowAssets" (Expr.mappingUint "marketTotalBorrowAssets" (Expr.param "id")),
        Stmt.letVar "totalBorrowShares" (Expr.mappingUint "marketTotalBorrowShares" (Expr.param "id")),
        Stmt.letVar "lastUpdate" (Expr.mappingUint "marketLastUpdate" (Expr.param "id")),
        Stmt.letVar "fee" (Expr.mappingUint "marketFee" (Expr.param "id")),
        Stmt.returnValues [
          Expr.localVar "totalSupplyAssets",
          Expr.localVar "totalSupplyShares",
          Expr.localVar "totalBorrowAssets",
          Expr.localVar "totalBorrowShares",
          Expr.localVar "lastUpdate",
          Expr.localVar "fee"
        ]
      ]
    },
    {
      name := "extSloads"
      params := [{ name := "slots", ty := .array .bytes32 }]
      returnType := none
      returns := [.array .uint256]
      body := [Stmt.returnStorageWords "slots"]
    },

    -- Owner/configuration
    {
      name := "setOwner"
      params := [{ name := "newOwner", ty := .address }]
      returnType := none
      body := [
        requireOwner,
        Stmt.require
          (Expr.logicalNot (Expr.eq (Expr.param "newOwner") (Expr.storage "owner")))
          "already set",
        Stmt.setStorage "owner" (Expr.param "newOwner"),
        Stmt.emit "SetOwner" [Expr.param "newOwner"],
        Stmt.stop
      ]
    },
    {
      name := "enableIrm"
      params := [{ name := "irm", ty := .address }]
      returnType := none
      body := [
        requireOwner,
        Stmt.require
          (Expr.eq (Expr.mapping "isIrmEnabled" (Expr.param "irm")) (Expr.literal 0))
          "already set",
        Stmt.setMapping "isIrmEnabled" (Expr.param "irm") (Expr.literal 1),
        Stmt.emit "EnableIrm" [Expr.param "irm"],
        Stmt.stop
      ]
    },
    {
      name := "enableLltv"
      params := [{ name := "lltv", ty := .uint256 }]
      returnType := none
      body := [
        requireOwner,
        Stmt.require
          (Expr.eq (Expr.mappingUint "isLltvEnabled" (Expr.param "lltv")) (Expr.literal 0))
          "already set",
        Stmt.require (Expr.lt (Expr.param "lltv") (Expr.literal wad)) "max LLTV exceeded",
        Stmt.setMappingUint "isLltvEnabled" (Expr.param "lltv") (Expr.literal 1),
        Stmt.emit "EnableLltv" [Expr.param "lltv"],
        Stmt.stop
      ]
    },
    {
      name := "setFeeRecipient"
      params := [{ name := "newFeeRecipient", ty := .address }]
      returnType := none
      body := [
        requireOwner,
        Stmt.require
          (Expr.logicalNot (Expr.eq (Expr.param "newFeeRecipient") (Expr.storage "feeRecipient")))
          "already set",
        Stmt.setStorage "feeRecipient" (Expr.param "newFeeRecipient"),
        Stmt.emit "SetFeeRecipient" [Expr.param "newFeeRecipient"],
        Stmt.stop
      ]
    },
    {
      name := "setAuthorization"
      params := [
        { name := "authorized", ty := .address },
        { name := "newIsAuthorized", ty := .uint256 }
      ]
      returnType := none
      body := [
        Stmt.require
          (Expr.logicalNot (Expr.eq
            (Expr.mapping2 "isAuthorized" Expr.caller (Expr.param "authorized"))
            (Expr.param "newIsAuthorized")))
          "already set",
        Stmt.setMapping2 "isAuthorized" Expr.caller (Expr.param "authorized") (Expr.param "newIsAuthorized"),
        Stmt.stop
      ]
    },

    -- Market creation/config
    {
      name := "createMarket"
      params := [
        { name := "loanToken", ty := .address },
        { name := "collateralToken", ty := .address },
        { name := "oracle", ty := .address },
        { name := "irm", ty := .address },
        { name := "lltv", ty := .uint256 }
      ]
      returnType := none
      body := [
        Stmt.letVar "id" (marketIdExpr #[(Expr.param "loanToken"), (Expr.param "collateralToken"), (Expr.param "oracle"), (Expr.param "irm"), (Expr.param "lltv")]),
        Stmt.require (Expr.eq (Expr.mapping "isIrmEnabled" (Expr.param "irm")) (Expr.literal 1)) "IRM not enabled",
        Stmt.require (Expr.eq (Expr.mappingUint "isLltvEnabled" (Expr.param "lltv")) (Expr.literal 1)) "LLTV not enabled",
        Stmt.require (Expr.eq (Expr.mappingUint "marketLastUpdate" (Expr.localVar "id")) (Expr.literal 0)) "market already created",
        Stmt.setMappingUint "marketLastUpdate" (Expr.localVar "id") Expr.blockTimestamp,
        Stmt.setMappingUint "marketFee" (Expr.localVar "id") (Expr.literal 0),
        Stmt.setMappingUint "marketTotalSupplyAssets" (Expr.localVar "id") (Expr.literal 0),
        Stmt.setMappingUint "marketTotalSupplyShares" (Expr.localVar "id") (Expr.literal 0),
        Stmt.setMappingUint "marketTotalBorrowAssets" (Expr.localVar "id") (Expr.literal 0),
        Stmt.setMappingUint "marketTotalBorrowShares" (Expr.localVar "id") (Expr.literal 0),
        Stmt.setMappingUint "idToLoanToken" (Expr.localVar "id") (Expr.param "loanToken"),
        Stmt.setMappingUint "idToCollateralToken" (Expr.localVar "id") (Expr.param "collateralToken"),
        Stmt.setMappingUint "idToOracle" (Expr.localVar "id") (Expr.param "oracle"),
        Stmt.setMappingUint "idToIrm" (Expr.localVar "id") (Expr.param "irm"),
        Stmt.setMappingUint "idToLltv" (Expr.localVar "id") (Expr.param "lltv"),
        Stmt.emit "CreateMarket" [
          Expr.localVar "id",
          Expr.param "loanToken",
          Expr.param "collateralToken",
          Expr.param "oracle",
          Expr.param "irm",
          Expr.param "lltv"
        ],
        Stmt.stop
      ]
    },
    {
      name := "setFee"
      params := [
        { name := "loanToken", ty := .address },
        { name := "collateralToken", ty := .address },
        { name := "oracle", ty := .address },
        { name := "irm", ty := .address },
        { name := "lltv", ty := .uint256 },
        { name := "newFee", ty := .uint256 }
      ]
      returnType := none
      body := [
        requireOwner,
        Stmt.letVar "id" (marketIdExpr #[(Expr.param "loanToken"), (Expr.param "collateralToken"), (Expr.param "oracle"), (Expr.param "irm"), (Expr.param "lltv")]),
        Stmt.require
          (Expr.gt (Expr.mappingUint "marketLastUpdate" (Expr.localVar "id")) (Expr.literal 0))
          "market not created",
        Stmt.require (Expr.le (Expr.param "newFee") (Expr.literal maxFee)) "max fee exceeded",
        Stmt.setMappingUint "marketFee" (Expr.localVar "id") (Expr.param "newFee"),
        Stmt.emit "SetFee" [Expr.localVar "id", Expr.param "newFee"],
        Stmt.stop
      ]
    }
  ]
  events := [
    {
      name := "SetOwner"
      params := [
        { name := "newOwner", ty := .address, kind := .indexed }
      ]
    },
    {
      name := "SetFee"
      params := [
        { name := "id", ty := .bytes32, kind := .indexed },
        { name := "newFee", ty := .uint256, kind := .unindexed }
      ]
    },
    {
      name := "SetFeeRecipient"
      params := [
        { name := "newFeeRecipient", ty := .address, kind := .indexed }
      ]
    },
    {
      name := "EnableIrm"
      params := [
        { name := "irm", ty := .address, kind := .indexed }
      ]
    },
    {
      name := "EnableLltv"
      params := [
        { name := "lltv", ty := .uint256, kind := .unindexed }
      ]
    },
    {
      name := "CreateMarket"
      params := [
        { name := "id", ty := .bytes32, kind := .indexed },
        { name := "loanToken", ty := .address, kind := .unindexed },
        { name := "collateralToken", ty := .address, kind := .unindexed },
        { name := "oracle", ty := .address, kind := .unindexed },
        { name := "irm", ty := .address, kind := .unindexed },
        { name := "lltv", ty := .uint256, kind := .unindexed }
      ]
    }
  ]
}

/--
Function selectors aligned with IMorpho signatures. For tuple params in IMorpho,
selectors are intentionally taken from tuple signatures even though the current
ContractSpec expresses flattened static parameters.
-/
def morphoSelectors : List Nat := [
  0x3644e515, -- DOMAIN_SEPARATOR()
  0x8da5cb5b, -- owner()
  0x46904840, -- feeRecipient()
  0xf2b863ce, -- isIrmEnabled(address)
  0xb485f3b8, -- isLltvEnabled(uint256)
  0x65e4ad9e, -- isAuthorized(address,address)
  0x70ae92d2, -- nonce(address)
  0x0acefb7b, -- lastUpdate(bytes32)
  0x417737d6, -- totalSupplyAssets(bytes32)
  0x0d2db074, -- totalSupplyShares(bytes32)
  0x12022eb9, -- totalBorrowAssets(bytes32)
  0x3f13c692, -- totalBorrowShares(bytes32)
  0x27cdab06, -- fee(bytes32)
  0x2c3c9157, -- idToMarketParams(bytes32)
  0x93c52062, -- position(bytes32,address)
  0x5c60e39a, -- market(bytes32)
  0x7784c685, -- extSloads(bytes32[])
  0x13af4035, -- setOwner(address)
  0x5a64f51e, -- enableIrm(address)
  0x4d98a93b, -- enableLltv(uint256)
  0xe74b981b, -- setFeeRecipient(address)
  0xeecea000, -- setAuthorization(address,bool)
  0x8c1358a2, -- createMarket((address,address,address,address,uint256))
  0x2b4f013c  -- setFee((address,address,address,address,uint256),uint256)
]

end Morpho.Compiler.Spec

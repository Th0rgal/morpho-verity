"""Shared macro blocker regression case catalog."""

from __future__ import annotations

CORE_FLOW_FRONTEND_REGRESSION_CASES = (
  {
    "name": "calls_with_return",
    "blocker": "externalWithReturn",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (oracle : Address) : Unit := do
    let _ <- Calls.withReturn oracle 0 []
    pure ()
""",
    "expected": "unsupported do element",
  },
  {
    "name": "internal_call",
    "blocker": "internalCall",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f () : Unit := do
    let _ <- call g()
    pure ()

  function g () : Unit := do
    pure ()
""",
    "expected": "unsupported do element",
  },
  {
    "name": "callback",
    "blocker": "callbacks",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (onBehalf : Address, data : Bytes) : Unit := do
    Callbacks.callback onBehalf 0 [] data
""",
    "expected": "unsupported expression in verity_contract body",
  },
  {
    "name": "erc20_transfer",
    "blocker": "erc20",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (token : Address, receiver : Address, amount : Uint256) : Unit := do
    ERC20.safeTransfer token receiver amount
""",
    "expected": "unsupported expression in verity_contract body",
  },
  {
    "name": "erc20_transfer_from",
    "blocker": "erc20",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (token : Address, sender : Address, receiver : Address, amount : Uint256) : Unit := do
    ERC20.safeTransferFrom token sender receiver amount
""",
    "expected": "unsupported expression in verity_contract body",
  },
  {
    "name": "struct_member2_read",
    "blocker": "structMember2",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    position : Uint256 := slot 0

  function f (id : Uint256, user : Address) : Unit := do
    let shares := position[id][user]._0
    let _ := shares
    pure ()
""",
    "expected": "unsupported expression in verity_contract body",
  },
  {
    "name": "struct_member2_write",
    "blocker": "structMember2",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    position : Uint256 := slot 0

  function f (id : Uint256, user : Address, x : Uint256) : Unit := do
    position[id][user]._0 := x
    pure ()
""",
    "expected": "unsupported do element",
  },
  {
    "name": "memory_ops",
    "blocker": "memoryOps",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (x : Uint256) : Unit := do
    mstore 0 x
    let y := mload 0
    let _ := y
    pure ()
""",
    "expected": "unsupported do element",
  },
)

COLLATERAL_FRONTEND_REGRESSION_CASES = (
  {
    "name": "calls_with_return",
    "blocker": "externalWithReturn",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (oracle : Address) : Unit := do
    let _ <- Calls.withReturn oracle 0 []
    pure ()
""",
    "expected": "unsupported do element",
  },
  {
    "name": "internal_call",
    "blocker": "internalCall",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f () : Unit := do
    let _ <- call g()
    pure ()

  function g () : Unit := do
    pure ()
""",
    "expected": "unsupported do element",
  },
  {
    "name": "callback",
    "blocker": "callbacks",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (onBehalf : Address, data : Bytes) : Unit := do
    Callbacks.callback onBehalf 0 [] data
""",
    "expected": "unsupported expression in verity_contract body",
  },
  {
    "name": "erc20_transfer",
    "blocker": "erc20",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (token : Address, receiver : Address, amount : Uint256) : Unit := do
    ERC20.safeTransfer token receiver amount
""",
    "expected": "unsupported expression in verity_contract body",
  },
  {
    "name": "erc20_transfer_from",
    "blocker": "erc20",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (token : Address, sender : Address, receiver : Address, amount : Uint256) : Unit := do
    ERC20.safeTransferFrom token sender receiver amount
""",
    "expected": "unsupported expression in verity_contract body",
  },
  {
    "name": "struct_member2_read",
    "blocker": "structMember2",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    position : Uint256 := slot 0

  function f (id : Uint256, user : Address) : Unit := do
    let shares := position[id][user]._0
    let _ := shares
    pure ()
""",
    "expected": "unsupported expression in verity_contract body",
  },
  {
    "name": "struct_member2_write",
    "blocker": "structMember2",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    position : Uint256 := slot 0

  function f (id : Uint256, user : Address, x : Uint256) : Unit := do
    position[id][user]._0 := x
    pure ()
""",
    "expected": "unsupported do element",
  },
  {
    "name": "memory_ops",
    "blocker": "memoryOps",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (x : Uint256) : Unit := do
    mstore 0 x
    let y := mload 0
    let _ := y
    pure ()
""",
    "expected": "unsupported do element",
  },
)

ISSUE_FRONTEND_REGRESSION_CASES = {
  123: CORE_FLOW_FRONTEND_REGRESSION_CASES,
  124: COLLATERAL_FRONTEND_REGRESSION_CASES,
}

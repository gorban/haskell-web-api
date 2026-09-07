{-# LANGUAGE OverloadedStrings #-}

-- | The AHI-4C reference adapter owns only account-dialog presentation around
-- Harch's retained-action lifecycle.  It never reads, copies, stores, or
-- submits the original form values: the capture kernel keeps that bounded
-- envelope and performs its one permitted replay.  Nor does this adapter
-- reproduce the page-security GET; it invokes the navigation runtime's
-- installed capability before asking the kernel to replay.
--
-- The application uses a direct native-dialog open because recovery has no
-- standalone link invoker: the original protected form is its focus-return
-- target and native fallback remains that form's normal navigation.  A
-- generic automatic-dialog Harch control would be speculative beyond this
-- account-specific presentation, so the existing typed dialog markup and
-- framework-owned close/focus adapter remain the only shared dialog surface.
module WebApi.App.Reauthentication
  ( reauthenticationRuntimeAsset,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified

reauthenticationRuntimeAsset :: HarchWeb.RuntimeAsset
reauthenticationRuntimeAsset =
  HarchWeb.RuntimeAsset
    { HarchWeb.runtimeAssetName = "web-api-reauthentication",
      HarchWeb.runtimeAssetPath = "/assets/reauthentication.js",
      HarchWeb.runtimeAssetScript = reauthenticationRuntimeScript
    }

reauthenticationRuntimeScript :: Text
reauthenticationRuntimeScript =
  Text.unlines
    [ "(() => {",
      "  const dialogSelector = 'dialog[data-web-api-reauthentication-dialog]';",
      "  const retrySelector = '[data-web-api-reauthentication-retry]';",
      "  const statusSelector = '[data-web-api-reauthentication-status]';",
      "  const readyCopySelector = '[data-web-api-reauthentication-ready-copy]';",
      "  let activeRecovery = null;",
      "",
      "  const captureKernel = () => window.__harchCaptureKernel;",
      "  const recoveryDialog = () => document.querySelector(dialogSelector);",
      "  const focusReturn = (recovery) => {",
      "    if (recovery?.invoker?.isConnected) { recovery.invoker.focus(); }",
      "  };",
      "  const clearConfirmation = (dialog) => {",
      "    const retry = dialog.querySelector(retrySelector);",
      "    const status = dialog.querySelector(statusSelector);",
      "    if (retry instanceof HTMLButtonElement) { retry.hidden = true; retry.disabled = false; }",
      "    if (status instanceof HTMLElement) { status.hidden = true; status.textContent = ''; }",
      "  };",
      "  const closeDialog = (dialog) => { if (dialog.open) { dialog.close(); } };",
      "  const disposeRecovery = (recovery) => {",
      "    if (activeRecovery?.actionId === recovery.actionId) { activeRecovery = null; }",
      "    captureKernel()?.cancel(recovery.actionId);",
      "  };",
      "  const finishRecovery = (recovery) => {",
      "    if (activeRecovery?.actionId === recovery.actionId) { activeRecovery = null; }",
      "    closeDialog(recovery.dialog);",
      "    focusReturn(recovery);",
      "  };",
      "  const startRecovery = (actionId) => {",
      "    const dialog = recoveryDialog();",
      "    if (!(dialog instanceof HTMLDialogElement)) { captureKernel()?.cancel(actionId); return; }",
      "    if (activeRecovery) { disposeRecovery(activeRecovery); closeDialog(dialog); }",
      "    const invoker = document.activeElement instanceof HTMLElement ? document.activeElement : null;",
      "    activeRecovery = { actionId, dialog, invoker };",
      "    clearConfirmation(dialog);",
      "    if (!dialog.open) { dialog.showModal(); }",
      "    dialog.querySelector('#login-identifier')?.focus();",
      "  };",
      "  const offerReplay = (recovery) => {",
      "    const retry = recovery.dialog.querySelector(retrySelector);",
      "    const status = recovery.dialog.querySelector(statusSelector);",
      "    const readyCopy = recovery.dialog.querySelector(readyCopySelector)?.dataset.webApiReauthenticationReadyCopy;",
      "    if (!(retry instanceof HTMLButtonElement) || !(status instanceof HTMLElement) || !readyCopy) { disposeRecovery(recovery); finishRecovery(recovery); return; }",
      "    status.textContent = readyCopy;",
      "    status.hidden = false;",
      "    retry.hidden = false;",
      "    retry.focus();",
      "  };",
      "  const replayAfterSecurityRefresh = async (recovery) => {",
      "    const kernel = captureKernel();",
      "    if (activeRecovery?.actionId === recovery.actionId) { activeRecovery = null; }",
      "    closeDialog(recovery.dialog);",
      "    try {",
      "      const refreshed = await kernel?.refreshPageSecurityForRetainedAction?.();",
      "      if (refreshed && kernel?.replayRetained(recovery.actionId)) { return; }",
      "    } catch (_error) {",
      "      // The kernel's cancellation rail restores ordinary form recovery.",
      "    }",
      "    kernel?.cancel(recovery.actionId);",
      "    focusReturn(recovery);",
      "  };",
      "",
      "  document.addEventListener('harch:action-reauthentication-required', (event) => {",
      "    const actionId = event.detail?.actionId;",
      "    if (typeof actionId === 'string' && actionId.length > 0) { startRecovery(actionId); }",
      "  });",
      "  document.addEventListener('harch:action-reauthentication-completed', (event) => {",
      "    if (!activeRecovery) { return; }",
      "    event.preventDefault();",
      "    offerReplay(activeRecovery);",
      "  });",
      "  document.addEventListener('harch:action-reauthentication-expired', (event) => {",
      "    if (activeRecovery?.actionId !== event.detail?.actionId) { return; }",
      "    const recovery = activeRecovery;",
      "    activeRecovery = null;",
      "    closeDialog(recovery.dialog);",
      "    focusReturn(recovery);",
      "  });",
      "  document.addEventListener('harch:navigation-start', () => {",
      "    if (!activeRecovery) { return; }",
      "    const recovery = activeRecovery;",
      "    activeRecovery = null;",
      "    closeDialog(recovery.dialog);",
      "  });",
      "  document.addEventListener('close', (event) => {",
      "    if (!(event.target instanceof HTMLDialogElement) || event.target !== activeRecovery?.dialog) { return; }",
      "    const recovery = activeRecovery;",
      "    activeRecovery = null;",
      "    captureKernel()?.cancel(recovery.actionId);",
      "    focusReturn(recovery);",
      "  }, true);",
      "  document.addEventListener('click', (event) => {",
      "    const retry = event.target instanceof Element ? event.target.closest(retrySelector) : null;",
      "    if (!(retry instanceof HTMLButtonElement) || retry.hidden || !activeRecovery) { return; }",
      "    event.preventDefault();",
      "    retry.disabled = true;",
      "    void replayAfterSecurityRefresh(activeRecovery);",
      "  });",
      "})();"
    ]

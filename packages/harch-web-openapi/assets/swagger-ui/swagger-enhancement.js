// Package-owned page enhancement for the typed Swagger UI surface
// (AHI-4E). Loaded by the framework's navigation runtime as
// script[data-harch-page-enhancement="harch-swagger-ui"]; the kernel imports
// this module and calls setupPageEnhancement exactly once per document,
// then invokes the returned disposer before the next enhanced navigation
// replaces the page. Module top level is deliberately side-effect free.
//
// CSP posture (see ../README.md): everything here runs under the framework's
// default script-src 'self'; style-src 'self' policy. The Swagger bundle is
// a classic self-hosted script injected once by this module (src-based
// insertion is 'self'-allowed), Swagger's inline styling goes through CSSOM,
// and no runtime <style> element or inline script is created.
//
// Authentication assistance (AHI-4E): the UI exposes only "None" or one
// editable complete Authorization header value, and an optional automatic
// OAuth client-credentials panel. Bearer state is memory-only (a closure
// variable plus Swagger's memory-only request state): this module reads no
// cookies, touches no local/session storage, and retains no automatic-flow
// history or snippets. The client secret is a one-use UI value: cleared
// from the input, the DOM, and module state after a successful exchange,
// replaced by a fixed-length placeholder that reveals neither value nor
// length, and announced through a polite live region.

const BUNDLE_FLAG = "harchSwaggerBundleLoaded";
const SECRET_PLACEHOLDER = "********";

export function setupPageEnhancement(root) {
  const mount = root.querySelector("[data-swagger-ui]");
  if (!mount) return undefined;

  const specUrl = mount.dataset.swaggerSpecUrl || "/docs/openapi.json";
  const bundleUrl = mount.dataset.swaggerBundleUrl || "/docs/assets/swagger-ui-bundle.js";
  const tokenEndpoint = mount.dataset.swaggerTokenEndpoint || "/api/oauth/token";
  const exampleClientId = mount.dataset.swaggerExampleClientId || "";
  const exampleClientSecret = mount.dataset.swaggerExampleClientSecret || "";
  const head = root.ownerDocument.head ?? document.head;

  // Memory-only request state: never persisted, never mirrored into storage.
  let authorizationHeaderValue = "";

  const panel = buildAuthPanel(root.ownerDocument, {
    tokenEndpoint,
    exampleClientId,
    exampleClientSecret,
    onAuthorizationChange(value) {
      authorizationHeaderValue = value;
    },
  });
  mount.before(panel.element);

  // The static, script-free fallback the server already rendered must
  // survive until the bundle arrives; record readiness only on success.
  let disposed = false;

  const loadBundle = () =>
    new Promise((resolve, reject) => {
      const existing = head.querySelector(`script[data-swagger-bundle="true"]`);
      if (existing) {
        if (window.SwaggerUIBundle) {
          resolve();
        } else {
          existing.addEventListener("load", () => resolve(), { once: true });
          existing.addEventListener("error", () => reject(new Error("swagger bundle failed")), { once: true });
        }
        return;
      }
      const script = root.ownerDocument.createElement("script");
      script.src = bundleUrl;
      script.dataset.swaggerBundle = "true";
      script.addEventListener("load", () => resolve(), { once: true });
      script.addEventListener("error", () => reject(new Error("swagger bundle failed")), { once: true });
      head.appendChild(script);
    });

  loadBundle()
    .then(() => {
      if (disposed || typeof window.SwaggerUIBundle !== "function") return;
      window.SwaggerUIBundle({
        url: specUrl,
        domNode: mount,
        deepLinking: true,
        docExpansion: "list",
        defaultModelsExpandDepth: 0,
        tryItOutEnabled: true,
        persistAuthorization: false,
        // The manually supplied header (or the automatic OAuth bearer) is
        // sent even when an HttpOnly cookie also exists, so users can test
        // same-token acceptance and conflict rejection deliberately.
        requestInterceptor(request) {
          if (authorizationHeaderValue) {
            request.headers = { ...(request.headers || {}), Authorization: authorizationHeaderValue };
          }
          return request;
        },
      });
      mount.dataset.swaggerReady = "true";
    })
    .catch(() => {
      // Keep the server-rendered fallback visible on failure; never leave a
      // half-initialized UI behind.
      if (!disposed) mount.dataset.swaggerFailed = "true";
    });

  return () => {
    disposed = true;
    authorizationHeaderValue = "";
    panel.dispose();
    if (mount.dataset.swaggerReady === "true") {
      // Unmount the UI instance and drop its DOM so a later entry rebuilds
      // from the server-rendered fallback rather than reusing stale state.
      mount.replaceChildren();
    }
    delete mount.dataset.swaggerReady;
    delete mount.dataset.swaggerFailed;
  };
}

// Builds the authentication assistance panel. Everything here is
// deliberately ordinary DOM: no inline handlers (CSP), no storage, and a
// disposer that drops the panel and its state entirely.
function buildAuthPanel(document, { tokenEndpoint, exampleClientId, exampleClientSecret, onAuthorizationChange }) {
  const element = document.createElement("section");
  element.dataset.swaggerAuthPanel = "true";

  const modeLabel = document.createElement("label");
  modeLabel.textContent = "Authorization header (None or a complete value such as Bearer ey...):";
  const authorizationInput = document.createElement("input");
  authorizationInput.type = "text";
  authorizationInput.dataset.swaggerAuthorizationInput = "true";
  authorizationInput.value = "";
  authorizationInput.addEventListener("change", () => {
    const raw = authorizationInput.value.trim();
    onAuthorizationChange(raw === "None" || raw === "" ? "" : raw);
  });
  element.append(modeLabel, authorizationInput);

  const oauthHeading = document.createElement("p");
  oauthHeading.textContent = "Automatic OAuth (client credentials, memory-only):";
  element.append(oauthHeading);

  const clientIdLabel = document.createElement("label");
  clientIdLabel.textContent = exampleClientId ? `Client ID (example: ${exampleClientId}):` : "Client ID:";
  const clientIdInput = document.createElement("input");
  clientIdInput.type = "text";
  clientIdInput.dataset.swaggerClientIdInput = "true";
  // Example hints are labels only: values are never prefilled.
  const clientSecretLabel = document.createElement("label");
  clientSecretLabel.textContent = exampleClientSecret ? `Client secret (example: ${exampleClientSecret}):` : "Client secret:";
  const clientSecretInput = document.createElement("input");
  clientSecretInput.type = "password";
  clientSecretInput.dataset.swaggerClientSecretInput = "true";

  const exchangeButton = document.createElement("button");
  exchangeButton.type = "button";
  exchangeButton.dataset.swaggerTokenExchange = "true";
  exchangeButton.textContent = "Get bearer token";

  const announcement = document.createElement("p");
  announcement.dataset.swaggerAuthAnnouncement = "true";
  announcement.setAttribute("role", "status");
  announcement.setAttribute("aria-live", "polite");

  element.append(clientIdLabel, clientIdInput, clientSecretLabel, clientSecretInput, exchangeButton, announcement);

  const clearSecret = () => {
    // One-use secret: clear the input, its DOM value, and any module state;
    // the fixed placeholder reveals neither the value nor its length.
    clientSecretInput.value = SECRET_PLACEHOLDER;
    clientSecretInput.placeholder = SECRET_PLACEHOLDER;
    announcement.textContent = "Client secret cleared.";
  };

  exchangeButton.addEventListener("click", () => {
    const clientId = clientIdInput.value;
    const clientSecret = clientSecretInput.value;
    const body = new URLSearchParams();
    body.set("grant_type", "client_credentials");
    body.set("client_id", clientId);
    body.set("client_secret", clientSecret);
    fetch(tokenEndpoint, {
      method: "POST",
      headers: { "Content-Type": "application/x-www-form-urlencoded" },
      body: body.toString(),
      credentials: "same-origin",
    })
      .then((response) => (response.ok ? response.json() : Promise.reject(new Error("token exchange failed"))))
      .then((payload) => {
        if (typeof payload.access_token !== "string" || payload.access_token.length === 0) {
          throw new Error("token exchange failed");
        }
        // Memory-only bearer state; nothing is persisted anywhere.
        onAuthorizationChange(`Bearer ${payload.access_token}`);
        authorizationInput.value = `Bearer ${SECRET_PLACEHOLDER}`;
        clearSecret();
      })
      .catch(() => {
        announcement.textContent = "Token exchange failed; the secret was not cleared.";
      });
  });

  return {
    element,
    dispose() {
      onAuthorizationChange("");
      element.remove();
    },
  };
}

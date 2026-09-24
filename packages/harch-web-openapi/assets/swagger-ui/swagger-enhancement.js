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

const BUNDLE_FLAG = "harchSwaggerBundleLoaded";

export function setupPageEnhancement(root) {
  const mount = root.querySelector("[data-swagger-ui]");
  if (!mount) return undefined;

  const specUrl = mount.dataset.swaggerSpecUrl || "/docs/openapi.json";
  const bundleUrl = mount.dataset.swaggerBundleUrl || "/docs/assets/swagger-ui-bundle.js";
  const head = root.ownerDocument.head ?? document.head;

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
        // The task surface exposes no Swagger auth authority beyond the raw
        // Authorization header control Swagger provides natively; no OAuth
        // storage or cookie reading is configured here (the OAuth panel
        // slice configures its memory-only plugin explicitly).
        persistAuthorization: false,
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
    if (mount.dataset.swaggerReady === "true") {
      // Unmount the UI instance and drop its DOM so a later entry rebuilds
      // from the server-rendered fallback rather than reusing stale state.
      mount.replaceChildren();
    }
    delete mount.dataset.swaggerReady;
    delete mount.dataset.swaggerFailed;
  };
}
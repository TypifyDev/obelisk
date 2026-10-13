# Seed provenance

This directory was seeded on 2026-08-25 from:

- upstream: https://github.com/reflex-frp/reflex-dom
- sub-path: `reflex-dom-core/`
- commit  : 97f08ae82335b9645ffd9ca89bf1187033265a9f
- reason  : replaces the old `reflex-dom-core-effectful/` (which built against
            the deleted `reflex-effectful` shim). New forked shape will sit on
            `eff-reflex`'s `Reflex.Host.HeadlessEffectful` / `EffectfulHost`
            entry point.

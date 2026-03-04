# Workflow
- **ALWAYS** track tasks and progress in `plan.org`, adding more granular tasks in the hierarchy (as TODO subheadings and checkbox lists) when necessary before starting implementation.
- **ALWAYS** mark items as complete in the the plan when you finish them and tests pass.
- When asked to work or continue with no other context, pick the first item, either adding more granular tasks or implementing it as required.
- **DO NOT** create migrations, fallbacks or aliases when altering functionality. This is a new project with no users or API contracts. We don't have to worry about breaking things.
- **ALWAYS** commit and push your work at the end of implementing a feature.

# Development requirements
- Optics that mutate state or have side-effects **MUST** represent these with the monadic effect system instead of directly changing state or causing side-effects.
- Follow the optics laws where possible, and clearly document instances where they are broken.
- All features should have working tests. Tests should favour using temporary buffers and assume availability of any library required by the feature. Only data and remote network responses should be mocked.

# Documentation
- Maintain a short, snappy `README.org` with some impressive real-world examples that show real gains in simplicity and clarity from using and composing optics.
- Full documentation is in the form of an Emacs manual for each package, written in org-mode and exported to info.
- **ALWAYS** keep documentation up-to-date.

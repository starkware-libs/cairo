# Development cycle

To keep up with the fast development of the Cairo language and Starknet network while maintaining a secure
and healthy development process, **we organize our work in 3-week cycles**.
These cycles consist of **milestones** and **sprints.**

## 📍 Milestones

A milestone is a set of [issues](https://github.com/OpenZeppelin/cairo-contracts/issues) intended to be addressed.
We usually aim to have at least 4 milestones planned ahead (~12 weeks of work),
enough visibility for users to set realistic expectations on when a feature will be available,
and give us space to gauge demand.
We can also make better decisions and prioritization when we’re mindful of the bigger picture.

We organize issues in our [Github project](https://github.com/orgs/OpenZeppelin/projects/29) in the following views:

- [Current milestone](https://github.com/orgs/OpenZeppelin/projects/29/views/2) (now)
- [Next milestone](https://github.com/orgs/OpenZeppelin/projects/29/views/3): ~3 weeks from now
- [After milestone](https://github.com/orgs/OpenZeppelin/projects/29/views/4): ~6 weeks from now
- [Later milestone](https://github.com/orgs/OpenZeppelin/projects/29/views/5): ~9 weeks from now
- [Backlog](https://github.com/orgs/OpenZeppelin/projects/29/views/7): issues not assigned to any milestone
- [Good to tackle](https://github.com/orgs/OpenZeppelin/projects/29/views/10): issues not planned soon nor in progress, for contributors to take

## 🏁 Sprints

A sprint is a 3-week period of time in which we intend to complete a milestone.
Some milestones might extend or shorten a little bit if issues are finished ahead or after schedule,
while the end of a sprint marks the time to release whatever work has been finished to date,
i.e. “[the release train departs](https://github.com/OpenZeppelin/cairo-contracts/blob/main/RELEASING.md)”.

- We design milestones to take ~25 working days so we can tackle them in a single sprint
    1. 5 working days times 3 weeks/cycle per 3 devs
        - ⇒ 45 workdays/sprint
    2. we estimate we spend roughly ~1/3 of our time doing reviews
        - ⇒ 30 workdays/sprint
    3. we apply a 1/6 reduction to account for vacations, sickness, distractions, support, etc.
        - **⇒ 25 workdays/sprint**
- To do so, we categorize issues by size based on a rough estimate of how many working days we expect it to take
  - 🦔 tiny: ~0.5 days
  - 🐇 small: ~2.5 days
  - 🐂 medium: ~5 days
  - 🦑 large: ~10 days

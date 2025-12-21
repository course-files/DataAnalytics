---
name: Feature request
about: Suggest an idea for this project
title: "[FEATURE]"
labels: enhancement
assignees: course-files

---

**1. Feature Request Title**

-   A good title should clearly state the desired capability and the context in which it will be used.
-   Focus on what is missing, not how you intend to implement it.
    -   *Example:* “Add role-based access control for admin and read-only users”

**2. Problem Statement (The “Why”)**

-   What problem or limitation are you currently facing?
-   What task is difficult, inefficient, or impossible with the current system?
    -   *Example:* “Currently, all users have the same permissions, which makes it difficult to safely allow read-only access for auditors.”
-   If the problem is minor, please say so.

**3. Current Behaviour**

-   Describe how the system behaves currently in relation to this problem.
    -   *Example:* “Any authenticated user can create, update, and delete records.”
-   This establishes a baseline and prevents misunderstandings about whether the feature already exists in another form.

**4. Proposed Feature / Desired Behaviour**

-   Describe the feature you are requesting.
-   Focus on what the system should be able to do, not internal implementation details.
    -   *Example:* “Introduce configurable user roles with distinct permissions such as admin, editor, and viewer.”
-   If you have implementation ideas, please keep them brief and clearly label them as suggestions, not requirements.

**5. Use Cases and Value**

-   Who benefits from this feature?
-   In what real-world scenarios would it be used?
-   What measurable or practical value does it add?
    -   *Example:* “This would allow organizations to comply with internal audit requirements while reducing the risk of accidental data modification.”

**6. Scope and Boundaries**

-   What is explicitly in scope for this request?
-   What is out of scope?
    -   *Example:* “This request does not include single sign-on or external identity providers.”

**7. Alternatives Considered**

-   Have you considered other approaches or workarounds?
-   Why are they insufficient?
    -   *Example:* “We considered maintaining separate deployments per user type, but this introduces operational overhead.”

**8. Environment and Context (If Relevant)**

-   Is this feature request tied to a specific environment or deployment context?
    -   Operating system
    -   Software version
    -   Scale of usage (single user, small team, enterprise)
    -   *Example:* “This request is relevant for multi-user deployments running version 2.1.0 and above.”

**9. Potential Risks or Trade-Offs**

-   Are there performance, security, or complexity concerns? Please acknowledge them upfront.
    -   *Example:* “Introducing role checks may add minor overhead to request handling.”

**10. Specific Questions or Requests to Maintainers**

-   What kind of feedback are you seeking?
    -   Feasibility?
    -   Alignment with roadmap?
    -   Willingness to accept a pull request?
    -   *Example:* “Would this align with the project’s long-term roadmap, and would a pull request be welcome?”

Lastly, please be concise and polite 😊

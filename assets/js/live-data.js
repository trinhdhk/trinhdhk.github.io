(() => {
  const escapeHtml = (value) => {
    if (!value) return "";
    return String(value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;")
      .replace(/'/g, "&#039;");
  };

  const fetchJson = async (url) => {
    const res = await fetch(url, { cache: "no-store" });
    if (!res.ok) throw new Error(`Request failed: ${res.status}`);
    return res.json();
  };

  const isYamlUrl = (url) => {
    return url.endsWith(".yml") || url.endsWith(".yaml");
  };

  const fetchYaml = async (url) => {
    const res = await fetch(url, { cache: "no-store" });
    if (!res.ok) throw new Error(`Request failed: ${res.status}`);
    const text = await res.text();
    if (!window.jsyaml) {
      throw new Error("js-yaml not loaded");
    }
    return window.jsyaml.load(text);
  };

  const fetchData = async (url) => {
    return isYamlUrl(url) ? fetchYaml(url) : fetchJson(url);
  };

  const fetchWithFallback = async (primaryUrl, fallbackUrl) => {
    try {
      const data = await fetchData(primaryUrl);
      console.info("Live data fetched", { url: primaryUrl, ok: true });
      return data;
    } catch (err) {
      console.warn("Live data fetch failed", { url: primaryUrl, error: err?.message || String(err) });

      // If primary is a raw.githubusercontent URL that returned 404, try swapping branch name main<->master
      try {
        const errMsg = err?.message || String(err);
        if (errMsg.includes("404") && primaryUrl.includes("raw.githubusercontent.com")) {
          let alt = null;
          if (primaryUrl.includes("/main/")) alt = primaryUrl.replace("/main/", "/master/");
          else if (primaryUrl.includes("/master/")) alt = primaryUrl.replace("/master/", "/main/");
          if (alt) {
            try {
              const altData = await fetchData(alt);
              console.info("Live data fetched (branch swap)", { url: alt, ok: true });
              return altData;
            } catch (e) {
              console.warn("Branch-swap fetch also failed", { url: alt, error: e?.message || String(e) });
            }
          }
        }
      } catch (e) {
        console.warn("Error during branch-swap attempt", e?.message || String(e));
      }

      if (!fallbackUrl || fallbackUrl === primaryUrl) {
        throw err;
      }

      // Try configured fallback first
      let fallbackTried = false;
      try {
        const data = await fetchData(fallbackUrl);
        console.info("Live data fetched (fallback)", { url: fallbackUrl, ok: true });
        return data;
      } catch (e) {
        console.warn("Fallback fetch failed", { url: fallbackUrl, error: e?.message || String(e) });
        fallbackTried = true;
      }

      // If fallback is a repo-relative path, also try an absolute path from current origin
      try {
        if (!fallbackUrl.match(/^https?:\/\//) && (fallbackUrl.startsWith("data/") || fallbackUrl.startsWith("/data/"))) {
          const abs = (fallbackUrl.startsWith("/")) ? `${window.location.origin}${fallbackUrl}` : `${window.location.origin}/${fallbackUrl}`;
          const data2 = await fetchData(abs);
          console.info("Live data fetched (absolute fallback)", { url: abs, ok: true });
          return data2;
        }
      } catch (e2) {
        console.warn("Absolute-fallback fetch failed", { url: fallbackUrl, error: e2?.message || String(e2) });
      }

      if (fallbackTried) throw err;
      throw err;
    }
  };

  // Cache local profile so all render functions can access it
  let localProfileCache = {};
  const loadLocalProfile = async () => {
    const candidates = [];
    const liveProfileEl = document.getElementById("live-profile");
    if (liveProfileEl) {
      const explicitProfileUrl = liveProfileEl.getAttribute("data-profile-url");
      if (explicitProfileUrl) {
        candidates.push(explicitProfileUrl);
      }
      const crawlUrl = liveProfileEl.getAttribute("data-crawl-url");
      if (crawlUrl && crawlUrl.includes("/data/crawl/")) {
        candidates.push(crawlUrl.replace("/data/crawl/crawl.yml", "/data/profile.yml"));
      }
    }
    candidates.push(
      "data/profile.yml",
      "../data/profile.yml",
      "/data/profile.yml",
      "/docs/data/profile.yml",
      window.location.origin + "/data/profile.yml",
      window.location.origin + "/docs/data/profile.yml"
    );
    for (const c of candidates) {
      try {
        const p = c.includes("raw.githubusercontent.com")
          ? await fetchWithFallback(c, null)
          : await fetchYaml(c);
        if (p && Object.keys(p).length) {
          localProfileCache = p;
          return;
        }
      } catch (e) {
        // ignore and try next
      }
    }
    localProfileCache = {};
  };

  const renderProfile = async () => {
    const el = document.getElementById("live-profile");
    if (!el) return;
    const url = el.getAttribute("data-crawl-url");
    const fallback = el.getAttribute("data-fallback");
    const defaultPhoto = el.getAttribute("data-default-photo");
    const defaultPhotoAlt = el.getAttribute("data-default-photo-alt") || "Profile photo";
    const profileRoleOverride = el.getAttribute("data-profile-role");
    const linkScholar = el.getAttribute("data-link-scholar");
    const linkOrcid = el.getAttribute("data-link-orcid");
    const linkGithub = el.getAttribute("data-link-github");
    if (!url) return;

    try {
      const data = await fetchWithFallback(url, fallback);
      console.info("Profile payload", data);
      const profile = data.profile || {};
      // Prefer fields from repository `data/profile.yml` when available (from cached load)
      const localProfile = localProfileCache || {};
      const displayName = (localProfile && localProfile.name) ? localProfile.name : profile.name;
      let displayRole = "";
      if (profileRoleOverride) {
        displayRole = profileRoleOverride;
      } else if (localProfile && typeof localProfile.role === "string") {
        displayRole = localProfile.role;
      }
      const displayAffiliation = (localProfile && localProfile.affiliation) ? localProfile.affiliation : profile.affiliation;
      const displayEmail = (localProfile && localProfile.email) ? localProfile.email : profile.email;
      const displayLocation = (localProfile && localProfile.location) ? localProfile.location : profile.location;
      const displayPhotoUrl = (localProfile && localProfile.photo_url) ? localProfile.photo_url : (profile.photo_url || defaultPhoto);
      const displayPhotoAlt = (localProfile && localProfile.photo_alt) ? localProfile.photo_alt : (profile.photo_alt || defaultPhotoAlt);
      const displayBio = (localProfile && localProfile.bio) ? localProfile.bio : profile.bio;
      const displayKeywords = Array.isArray(localProfile.keywords)
        ? localProfile.keywords
        : [];
      const metrics = data.metrics || {};
      const metricRows = [
        { label: "Citations", value: metrics.citations },
        { label: "H-index", value: metrics.h_index },
        { label: "i10-index", value: metrics.i10_index }
      ].filter((item) => item.value);
      const metricHtml = metricRows
        .map((item) => `<div class=\"metric\"><span>${escapeHtml(item.label)}: </span><span> ${escapeHtml(item.value)}</span></div>`)
        .join("");

      const photoUrl = displayPhotoUrl || defaultPhoto;
      const photoHtml = photoUrl
        ? `<img class=\"profile-photo\" src=\"${escapeHtml(photoUrl)}\" alt=\"${escapeHtml(displayPhotoAlt)}\">`
        : "";
      const bioHtml = displayBio ? `<div class=\"bio\">${escapeHtml(displayBio)}</div>` : "";
      const keywords = displayKeywords || [];
      const keywordsHtml = keywords.length
        ? `<div class=\"keywords\"><strong>Keywords:</strong> ${escapeHtml(keywords.join(", "))}</div>`
        : "";
      const linksHtml = [
        linkScholar ? `<a href=\"${escapeHtml(linkScholar)}\" target=\"_blank\" rel=\"noopener\">Google Scholar</a>` : "",
        linkOrcid ? `<a href=\"${escapeHtml(linkOrcid)}\" target=\"_blank\" rel=\"noopener\">ORCID</a>` : "",
        linkGithub ? `<a href=\"${escapeHtml(linkGithub)}\" target=\"_blank\" rel=\"noopener\">GitHub</a>` : ""
      ].filter(Boolean).join("");

      const updatedAt = data.generated_at ? new Date(data.generated_at) : null;
      const updatedLabel = updatedAt && !Number.isNaN(updatedAt.getTime())
        ? new Intl.DateTimeFormat("en-GB", {
            day: "2-digit",
            month: "2-digit",
            year: "2-digit",
            hour: "2-digit",
            minute: "2-digit",
            second: "2-digit",
            hour12: false
          }).format(updatedAt).replace(",", "")
        : "";

      el.innerHTML = `
        <div class=\"profile-hero\">
          <div class=\"profile-layout\">
            <div>
              ${photoHtml}
            </div>
            <div class=\"profile-meta\">
              ${displayName ? `<br><strong>${escapeHtml(displayName)}</strong>` : ""}
              ${displayRole ? `<br>${escapeHtml(displayRole)}` : ""}
              ${displayAffiliation ? `<br>${escapeHtml(displayAffiliation)}` : ""}
              ${displayEmail ? `<br><a href=\"mailto:${escapeHtml(displayEmail)}\">${escapeHtml(displayEmail)}</a>` : ""}
              ${displayLocation ? `<br>${escapeHtml(displayLocation)}` : ""}
              ${bioHtml}
              ${keywordsHtml}
              ${linksHtml ? `<div class=\"profile-links\">${linksHtml}</div>` : ""}
            </div>
            <div class=\"profile-metrics\">
              <strong>Metrics</strong>
              ${metricHtml ? `<div class=\"metrics\">${metricHtml}</div>` : ""}
            </div>
          </div>
        </div>
        <div class=\"live-card\" style=\"margin-top: 0.75rem;\">
          <strong>Last updated:</strong> ${escapeHtml(updatedLabel)}
        </div>
      `;
    } catch (err) {
      el.textContent = "Failed to load live profile data.";
    }
  };

  const renderInterests = async () => {
    const el = document.getElementById("live-interests");
    if (!el) return;
    const url = el.getAttribute("data-crawl-url");
    const fallback = el.getAttribute("data-fallback");
    const defaults = el.getAttribute("data-default-interests") || "";
    if (!url) return;

    try {
      const data = await fetchWithFallback(url, fallback);
      const profile = data.profile || {};
      const interests = Array.isArray(profile.research_interests)
        ? profile.research_interests
        : (profile.research_interests ? [profile.research_interests] : []);
      const fallbackItems = defaults
        .split(",")
        .map((item) => item.trim())
        .filter((item) => item.length > 0);
      const items = interests.length ? interests : fallbackItems;

      if (!items.length) {
        el.textContent = "Research interests not available yet.";
        return;
      }

      const list = items
        .map((item) => `<li>${escapeHtml(item)}</li>`)
        .join("");

      el.innerHTML = `<ul class=\"interest-list\">${list}</ul>`;
    } catch (err) {
      el.textContent = "Failed to load research interests.";
    }
  };

  const renderFocusAreas = async () => {
    const el = document.getElementById("live-focus");
    if (!el) return;
    const url = el.getAttribute("data-crawl-url");
    const fallback = el.getAttribute("data-fallback");
    const defaults = el.getAttribute("data-default-focus") || "";
    if (!url) return;

    try {
      const data = await fetchWithFallback(url, fallback);
      const profile = data.profile || {};
      const focus = Array.isArray(profile.research_interests)
        ? profile.research_interests
        : (profile.research_interests ? [profile.research_interests] : []);
      const fallbackItems = defaults
        .split(",")
        .map((item) => item.trim())
        .filter((item) => item.length > 0);
      const items = focus.length ? focus : fallbackItems;

      if (!items.length) {
        el.textContent = "Focus areas not available yet.";
        return;
      }

      const list = items.map((item) => `<li>${escapeHtml(item)}</li>`).join("");
      el.innerHTML = `<ul class=\"interest-list\">${list}</ul>`;
    } catch (err) {
      el.textContent = "Failed to load focus areas.";
    }
  };

  const renderQualifications = async () => {
    const el = document.getElementById("live-qualifications");
    if (!el) return;
    const url = el.getAttribute("data-crawl-url");
    const fallback = el.getAttribute("data-fallback");
    const defaults = el.getAttribute("data-default-qualifications") || "";
    if (!url) return;

    try {
      const data = await fetchWithFallback(url, fallback);
      const items = Array.isArray(data.qualifications) ? data.qualifications : [];
      const fallbackItems = defaults
        .split(",")
        .map((item) => item.trim())
        .filter((item) => item.length > 0);
      const listItems = items.length ? items : fallbackItems;

      if (!listItems.length) {
        el.textContent = "Qualifications not available yet.";
        return;
      }

      const list = listItems.map((item) => `<li>${escapeHtml(item)}</li>`).join("");
      el.innerHTML = `<ul class=\"interest-list\">${list}</ul>`;
    } catch (err) {
      el.textContent = "Failed to load qualifications.";
    }
  };

  const renderPublications = async () => {
    const el = document.getElementById("live-publications");
    if (!el) return;
    const url = el.getAttribute("data-pubs-url");
    const fallback = el.getAttribute("data-fallback");
    if (!url) return;

    try {
      const data = await fetchWithFallback(url, fallback);
      console.info("Publications payload", { count: (data.items || []).length, data });
      const items = data.items || [];
      if (!items.length) {
        el.textContent = "No publications found yet.";
        return;
      }

      const rows = items
        .map((item) => {
          const title = escapeHtml(item.title || "Untitled");
          const authors = escapeHtml(item.authors || "");
          const venue = escapeHtml(item.venue || "");
          const year = escapeHtml(item.year || "");
          const sources = Array.isArray(item.sources)
            ? item.sources
            : (item.source ? [item.source] : []);
          const sourceLabel = sources.length ? sources.map(escapeHtml).join(", ") : "";
          const link = item.url ? `<a href=\"${escapeHtml(item.url)}\" target=\"_blank\" rel=\"noopener\">Link</a>` : "";

          return `
            <li>
              <strong>${title}</strong>
              ${authors ? `<br>${authors}` : ""}
              ${venue ? `<br>${venue}` : ""}
              ${year ? ` (${year})` : ""}
              ${sourceLabel ? `<br><em>${sourceLabel}</em>` : ""}
              ${link ? ` | ${link}` : ""}
            </li>
          `;
        })
        .join("");

      el.innerHTML = `<ul>${rows}</ul>`;
    } catch (err) {
      el.textContent = "Failed to load publications.";
    }
  };

  const renderWorkHistory = async () => {
    const el = document.getElementById("live-history");
    if (!el) return;
    const url = el.getAttribute("data-crawl-url");
    const fallback = el.getAttribute("data-fallback");
    if (!url) return;

    try {
      const data = await fetchWithFallback(url, fallback);
      const items = Array.isArray(data.work_history) ? data.work_history : [];

      if (!items.length) {
        el.textContent = "Work history not available yet.";
        return;
      }

      const rows = items
        .map((item) => {
          const role = escapeHtml(item.role || "");
          const org = escapeHtml(item.organization || "");
          const start = escapeHtml(item.start_date || "");
          const end = escapeHtml(item.end_date || "");
          const range = start || end ? `${start}${end ? ` - ${end}` : ""}` : "";
          const detail = [role, org].filter(Boolean).join(", ");
          return `<li>${detail}${range ? ` (${range})` : ""}</li>`;
        })
        .join("");

      el.innerHTML = `<ul>${rows}</ul>`;
    } catch (err) {
      el.textContent = "Failed to load work history.";
    }
  };

  const renderRepos = async () => {
    const el = document.getElementById("live-repos");
    if (!el) return;
    const user = el.getAttribute("data-github-user");
    if (!user) return;

    try {
      const url = `https://api.github.com/users/${user}/repos?per_page=12&sort=updated`;
      const repos = await fetchJson(url);

      if (!Array.isArray(repos) || repos.length === 0) {
        el.textContent = "No repositories found.";
        return;
      }

      const rows = repos
        .filter((repo) => !repo.fork)
        .map((repo) => {
          const name = escapeHtml(repo.name || "");
          const desc = escapeHtml(repo.description || "");
          const topics = Array.isArray(repo.topics) ? repo.topics.map(escapeHtml).join(", ") : "";
          const updated = repo.updated_at ? new Date(repo.updated_at).toLocaleDateString() : "";

          return `
            <li>
              <a href=\"${escapeHtml(repo.html_url)}\" target=\"_blank\" rel=\"noopener\">${name}</a>
              ${desc ? `<br>${desc}` : ""}
              ${topics ? `<br><small>Topics: ${topics}</small>` : ""}
              ${updated ? `<br><small>Updated: ${escapeHtml(updated)}</small>` : ""}
            </li>
          `;
        })
        .join("");

      el.innerHTML = `<ul>${rows}</ul>`;
    } catch (err) {
      el.textContent = "Failed to load repositories.";
    }
  };

  // Load local profile once, then render sections
  (async () => {
    await loadLocalProfile();
    await renderProfile();
    renderFocusAreas();
    renderWorkHistory();
    renderInterests();
    renderQualifications();
    renderPublications();
    renderRepos();
  })();
})();

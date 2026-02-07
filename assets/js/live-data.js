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
      if (!fallbackUrl || fallbackUrl === primaryUrl) {
        throw err;
      }
      const data = await fetchData(fallbackUrl);
      console.info("Live data fetched (fallback)", { url: fallbackUrl, ok: true });
      return data;
    }
  };

  const renderProfile = async () => {
    const el = document.getElementById("live-profile");
    if (!el) return;
    const url = el.getAttribute("data-crawl-url");
    const fallback = el.getAttribute("data-fallback");
    const defaultPhoto = el.getAttribute("data-default-photo");
    const defaultPhotoAlt = el.getAttribute("data-default-photo-alt") || "Profile photo";
    const linkScholar = el.getAttribute("data-link-scholar");
    const linkOrcid = el.getAttribute("data-link-orcid");
    const linkGithub = el.getAttribute("data-link-github");
    if (!url) return;

    try {
      const data = await fetchWithFallback(url, fallback);
      console.info("Profile payload", data);
      const profile = data.profile || {};
      // Prefer name from repository `data/profile.yml` when available
      let localProfile = {};
      try {
        localProfile = await fetchYaml("data/profile.yml");
      } catch (e) {
        localProfile = {};
      }
      const displayName = (localProfile && localProfile.name) ? localProfile.name : profile.name;
      const metrics = data.metrics || {};
      const metricRows = [
        { label: "Citations", value: metrics.citations },
        { label: "H-index", value: metrics.h_index },
        { label: "i10-index", value: metrics.i10_index }
      ].filter((item) => item.value);
      const metricHtml = metricRows
        .map((item) => {
          return `<div class="metric"><span>${escapeHtml(item.label)}: </span><span> ${escapeHtml(item.value)}</span></div>`;
        })
        .join("");

      const photoUrl = profile.photo_url || defaultPhoto;
      const photoHtml = photoUrl
        ? `<img class="profile-photo" src="${escapeHtml(photoUrl)}" alt="${escapeHtml(profile.photo_alt || defaultPhotoAlt)}">`
        : "";
      const bioHtml = profile.bio ? `<div class="bio">${escapeHtml(profile.bio)}</div>` : "";
      const interests = Array.isArray(profile.research_interests)
        ? profile.research_interests
        : (profile.research_interests ? [profile.research_interests] : []);
      const interestsHtml = interests.length
        ? `<div class="interests"><strong>Research interests:</strong> ${escapeHtml(interests.join(", "))}</div>`
        : "";
      const linksHtml = [
        linkScholar ? `<a href="${escapeHtml(linkScholar)}" target="_blank" rel="noopener">Google Scholar</a>` : "",
        linkOrcid ? `<a href="${escapeHtml(linkOrcid)}" target="_blank" rel="noopener">ORCID</a>` : "",
        linkGithub ? `<a href="${escapeHtml(linkGithub)}" target="_blank" rel="noopener">GitHub</a>` : ""
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
        <div class="profile-hero">
          <div class="profile-layout">
            <div>
              ${photoHtml}
            </div>
            <div style="display: inline-table">
              ${escapeHtml(profile.role || "")}
              ${displayName ? `<br><strong>${escapeHtml(displayName)}</strong>` : ""}
              ${profile.affiliation ? `<br>${escapeHtml(profile.affiliation)}` : ""}
              ${profile.email ? `<br><a href="mailto:${escapeHtml(profile.email)}">${escapeHtml(profile.email)}</a>` : ""}
              ${profile.location ? `<br>${escapeHtml(profile.location)}` : ""}
              ${bioHtml}
              ${interestsHtml}
              ${linksHtml ? `<div class="profile-links">${linksHtml}</div>` : ""}
            </div>
            <aside class="profile-metrics">
              <strong>Metrics</strong>
              ${metricHtml ? `<div class="metrics">${metricHtml}</div>` : ""}
            </aside>
          </div>
        </div>
        <div class="live-card" style="margin-top: 0.75rem;">
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

      el.innerHTML = `<ul class="interest-list">${list}</ul>`;
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
      el.innerHTML = `<ul class="interest-list">${list}</ul>`;
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
      el.innerHTML = `<ul class="interest-list">${list}</ul>`;
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
          const link = item.url ? `<a href="${escapeHtml(item.url)}" target="_blank" rel="noopener">Link</a>` : "";

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
              <a href="${escapeHtml(repo.html_url)}" target="_blank" rel="noopener">${name}</a>
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

  renderProfile();
  renderFocusAreas();
  renderWorkHistory();
  renderInterests();
  renderQualifications();
  renderPublications();
  renderRepos();
})();

/* extension.js
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

import Gio from 'gi://Gio';
import St from 'gi://St';

import * as Main from 'resource:///org/gnome/shell/ui/main.js';
import {Extension, gettext as _} from 'resource:///org/gnome/shell/extensions/extension.js';

//const SearchResults = Main.overview._overview._controls._searchController._searchResults;


class SearchProvider {

    constructor(extension) {
        this._extension = extension;
        this.searchEngineIcons = extension.searchEngineIcons;
    }
    /**
     * The application of the provider.
     *
     * Applications will return a `Gio.AppInfo` representing themselves.
     * Extensions will usually return `null`.
     *
     * @type {Gio.AppInfo}
     */
    get appInfo() {
        return null;
    }

    /**
     * Whether the provider offers detailed results.
     *
     * Applications will return `true` if they have a way to display more
     * detailed or complete results. Extensions will usually return `false`.
     *
     * @type {boolean}
     */
    get canLaunchSearch() {
        return true;
    }

    /**
     * The unique ID of the provider.
     *
     * Applications will return their application ID. Extensions will usually
     * return their UUID.
     *
     * @type {string}
     */
    get id() {
        return this._extension.uuid;
    }
    
    /**
     * Launch the search result.
     *
     * This method is called when a search provider result is activated.
     *
     * @param {string} result - The result identifier
     * @param {string[]} terms - The search terms
     */
    activateResult(result, terms) {
        const context = new Gio.AppLaunchContext;

        this.settings = this._extension.getSettings();
        const searchEngine = this.settings.get_int('search-engine');

        var cmd = `xdg-open `
        const urlRegex = /(https?:\/\/)?(www\.)?[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()@:%_\+.~#?&\/\/=]*)/;
        if(urlRegex.test(terms.join(" ")) && terms.join(" ").match(urlRegex)[0].length == terms.join(" ").length) {
            if(terms.join(" ").startsWith("http://") || terms.join(" ").startsWith("https://")) {
                cmd += terms.join(" ")
            } else {
                cmd += `https://${terms.join(" ")}`
            }
        } else {
            cmd += `"${this._extension.searchEngineUrls[searchEngine]}${terms.join('+')}"`
        }
        Gio.AppInfo.create_from_commandline(cmd, null, 2).launch([], context);
    }

    /**
     * Launch the search provider.
     *
     * This method is called when a search provider is activated. A provider can
     * only be activated if the `appInfo` property holds a valid `Gio.AppInfo`
     * and the `canLaunchSearch` property is `true`.
     *
     * Applications will typically open a window to display more detailed or
     * complete results.
     *
     * @param {string[]} terms - The search terms
     */
    launchSearch(terms) {
        return null;
    }

    /**
     * Create a result object.
     *
     * This method is called to create an actor to represent a search result.
     *
     * Implementations may return any `Clutter.Actor` to serve as the display
     * result, or `null` for the default implementation.
     *
     * @param {ResultMeta} meta - A result metadata object
     * @returns {Clutter.Actor} An actor for the result
     */
    createResultObject(meta) {
        return null;
    }

    /**
     * Get result metadata.
     *
     * This method is called to get a `ResultMeta` for each identifier.
     *
     * @param {string[]} results - The result identifiers
     * @param {Gio.Cancellable} [cancellable] - A cancellable for the operation
     * @returns {Promise<ResultMeta[]>} A list of result metadata objects
     */
    getResultMetas(results, cancellable = null) {

        // Choose icon based on settings
        this.settings = this._extension.getSettings();
        const searchEngine = this.settings.get_int('search-engine');
        const gicon = Gio.icon_new_for_string(this._extension.path + '/' + this.searchEngineIcons[searchEngine]);
        const { scaleFactor } = St.ThemeContext.get_for_stage(global.stage);

        return new Promise((resolve, reject) => {
            const resultMetas = [];

            for (let identifier of results) {
                const meta = {
                    id: identifier,
                    name: 'Web Search',
                    description: 'Launch web search',
                    createIcon: (size) => {
                        let ic = new St.Icon({
                            gicon,
                            width: size * scaleFactor,
                            height: size * scaleFactor,
                            icon_size: size * scaleFactor
                        })
                        return ic;
                    }
                };
                resultMetas.push(meta);
            }
            resolve(resultMetas);
        });
    }

    /**
     * Initiate a new search.
     *
     * This method is called to start a new search and should return a list of
     * unique identifiers for the results.
     *
     * @param {string[]} terms - The search terms
     * @param {Gio.Cancellable} [cancellable] - A cancellable for the operation
     * @returns {Promise<string[]>} A list of result identifiers
     */
    getInitialResultSet(terms, cancellable = null) {
        return new Promise((resolve, reject) => {
            const identifiers = ['Web Search'];

            resolve(identifiers);
        });
    }

    /**
     * Refine the current search.
     *
     * This method is called to refine the current search results with
     * expanded terms and should return a subset of the original result set.
     *
     * Implementations may use this method to refine the search results more
     * efficiently than running a new search, or simply pass the terms to the
     * implementation of `getInitialResultSet()`.
     *
     * @param {string[]} results - The original result set
     * @param {string[]} terms - The search terms
     * @param {Gio.Cancellable} [cancellable] - A cancellable for the operation
     * @returns {Promise<string[]>}
     */
    getSubsearchResultSet(results, terms, cancellable = null) {
        return this.getInitialResultSet(terms, cancellable);
    }

    /**
     * Filter the current search.
     *
     * This method is called to truncate the number of search results.
     *
     * Implementations may use their own criteria for discarding results, or
     * simply return the first n-items.
     *
     * @param {string[]} results - The original result set
     * @param {number} maxResults - The maximum amount of results
     * @returns {string[]} The filtered results
     */
    filterResults(results, maxResults) {
        if (results.length <= maxResults)
            return results;

        return results.slice(0, maxResults);
    }
}

export default class MyExtension extends Extension {
    constructor(meta) {
        super(meta);
        this._provider = null;
        this._decoder = null;
        this.searchEngineUrls;
        this.searchEngineIcons;
    }

    _setSearchEngines() {
        this._decoder = new TextDecoder();
        const searchEngineFile = this.dir.get_child('search-engines.json');
        const [, contents, etag] = searchEngineFile.load_contents(null);
        const json = JSON.parse(this._decoder.decode(contents));
        this.searchEngineUrls = json.map(d => d.url);
        this.searchEngineIcons = json.map(d => d.icon);
    }

    _deleteSearchEngines() {
        this._decoder = null;
        this.searchEngineUrls = null;
        this.searchEngineIcons = null;
    }
    
    enable() {
        if (this._decoder === null) {
            this._setSearchEngines();
        }
        if (this._provider === null) {
            this._provider = new SearchProvider(this);
            Main.overview.searchController.addProvider(this._provider);
        }
    }

    disable() {
        if (this._provider instanceof SearchProvider) {
            Main.overview.searchController.removeProvider(this._provider);
            this._provider = null;
        }
        this._deleteSearchEngines();
    }
}

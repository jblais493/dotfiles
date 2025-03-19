import Adw from 'gi://Adw';
import Gio from 'gi://Gio';
import Gtk from 'gi://Gtk';

import {ExtensionPreferences} from 'resource:///org/gnome/Shell/Extensions/js/extensions/prefs.js';

export default class MyExtensionPreferences extends ExtensionPreferences {
    fillPreferencesWindow(window) {
        const settings = this.getSettings();

        // Create a preferences page, with a single group
        const page = new Adw.PreferencesPage();
        window.add(page);

        const group = new Adw.PreferencesGroup();
        page.add(group);

        // Loading list of search engines
        const searchEngineFile = this.dir.get_child('search-engines.json');
        const [, contents, etag] = searchEngineFile.load_contents(null);
        const decoder = new TextDecoder();
        const json = JSON.parse(decoder.decode(contents));
        const searchEngineNames = json.map(d => d.name);

        const list = new Gtk.StringList(); 
        searchEngineNames.forEach(d => list.append(d)); // Not using strings attribute for compatibility with gnome43
        
        const dropdown = new Adw.ComboRow({
            title: 'Search engine',
            model: list,
            selected: settings.get_enum('search-engine')
        });

        settings.bind('search-engine', dropdown, 'selected',
            Gio.SettingsBindFlags.DEFAULT);

        // Add the switch to the group
        group.add(dropdown);

        // Make sure the window doesn't outlive the settings object
        window._settings = settings;
    }
}

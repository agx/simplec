/* Automatically generated by remote_generate_stubs.pl.
 * Do not edit this file.  Any changes you make will be lost.
 */

{   /* (unused) => 0 */
    .fn = NULL,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* Open => 1 */
    .fn = (dispatch_fn) remoteDispatchOpen,
    .args_filter = (xdrproc_t) xdr_remote_open_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* Close => 2 */
    .fn = (dispatch_fn) remoteDispatchClose,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* GetType => 3 */
    .fn = (dispatch_fn) remoteDispatchGetType,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_get_type_ret,
},
{   /* GetVersion => 4 */
    .fn = (dispatch_fn) remoteDispatchGetVersion,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_get_version_ret,
},
{   /* GetMaxVcpus => 5 */
    .fn = (dispatch_fn) remoteDispatchGetMaxVcpus,
    .args_filter = (xdrproc_t) xdr_remote_get_max_vcpus_args,
    .ret_filter = (xdrproc_t) xdr_remote_get_max_vcpus_ret,
},
{   /* NodeGetInfo => 6 */
    .fn = (dispatch_fn) remoteDispatchNodeGetInfo,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_node_get_info_ret,
},
{   /* GetCapabilities => 7 */
    .fn = (dispatch_fn) remoteDispatchGetCapabilities,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_get_capabilities_ret,
},
{   /* DomainAttachDevice => 8 */
    .fn = (dispatch_fn) remoteDispatchDomainAttachDevice,
    .args_filter = (xdrproc_t) xdr_remote_domain_attach_device_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainCreate => 9 */
    .fn = (dispatch_fn) remoteDispatchDomainCreate,
    .args_filter = (xdrproc_t) xdr_remote_domain_create_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainCreateXml => 10 */
    .fn = (dispatch_fn) remoteDispatchDomainCreateXml,
    .args_filter = (xdrproc_t) xdr_remote_domain_create_xml_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_create_xml_ret,
},
{   /* DomainDefineXml => 11 */
    .fn = (dispatch_fn) remoteDispatchDomainDefineXml,
    .args_filter = (xdrproc_t) xdr_remote_domain_define_xml_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_define_xml_ret,
},
{   /* DomainDestroy => 12 */
    .fn = (dispatch_fn) remoteDispatchDomainDestroy,
    .args_filter = (xdrproc_t) xdr_remote_domain_destroy_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainDetachDevice => 13 */
    .fn = (dispatch_fn) remoteDispatchDomainDetachDevice,
    .args_filter = (xdrproc_t) xdr_remote_domain_detach_device_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainDumpXml => 14 */
    .fn = (dispatch_fn) remoteDispatchDomainDumpXml,
    .args_filter = (xdrproc_t) xdr_remote_domain_dump_xml_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_dump_xml_ret,
},
{   /* DomainGetAutostart => 15 */
    .fn = (dispatch_fn) remoteDispatchDomainGetAutostart,
    .args_filter = (xdrproc_t) xdr_remote_domain_get_autostart_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_get_autostart_ret,
},
{   /* DomainGetInfo => 16 */
    .fn = (dispatch_fn) remoteDispatchDomainGetInfo,
    .args_filter = (xdrproc_t) xdr_remote_domain_get_info_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_get_info_ret,
},
{   /* DomainGetMaxMemory => 17 */
    .fn = (dispatch_fn) remoteDispatchDomainGetMaxMemory,
    .args_filter = (xdrproc_t) xdr_remote_domain_get_max_memory_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_get_max_memory_ret,
},
{   /* DomainGetMaxVcpus => 18 */
    .fn = (dispatch_fn) remoteDispatchDomainGetMaxVcpus,
    .args_filter = (xdrproc_t) xdr_remote_domain_get_max_vcpus_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_get_max_vcpus_ret,
},
{   /* DomainGetOsType => 19 */
    .fn = (dispatch_fn) remoteDispatchDomainGetOsType,
    .args_filter = (xdrproc_t) xdr_remote_domain_get_os_type_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_get_os_type_ret,
},
{   /* DomainGetVcpus => 20 */
    .fn = (dispatch_fn) remoteDispatchDomainGetVcpus,
    .args_filter = (xdrproc_t) xdr_remote_domain_get_vcpus_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_get_vcpus_ret,
},
{   /* ListDefinedDomains => 21 */
    .fn = (dispatch_fn) remoteDispatchListDefinedDomains,
    .args_filter = (xdrproc_t) xdr_remote_list_defined_domains_args,
    .ret_filter = (xdrproc_t) xdr_remote_list_defined_domains_ret,
},
{   /* DomainLookupById => 22 */
    .fn = (dispatch_fn) remoteDispatchDomainLookupById,
    .args_filter = (xdrproc_t) xdr_remote_domain_lookup_by_id_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_lookup_by_id_ret,
},
{   /* DomainLookupByName => 23 */
    .fn = (dispatch_fn) remoteDispatchDomainLookupByName,
    .args_filter = (xdrproc_t) xdr_remote_domain_lookup_by_name_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_lookup_by_name_ret,
},
{   /* DomainLookupByUuid => 24 */
    .fn = (dispatch_fn) remoteDispatchDomainLookupByUuid,
    .args_filter = (xdrproc_t) xdr_remote_domain_lookup_by_uuid_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_lookup_by_uuid_ret,
},
{   /* NumOfDefinedDomains => 25 */
    .fn = (dispatch_fn) remoteDispatchNumOfDefinedDomains,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_num_of_defined_domains_ret,
},
{   /* DomainPinVcpu => 26 */
    .fn = (dispatch_fn) remoteDispatchDomainPinVcpu,
    .args_filter = (xdrproc_t) xdr_remote_domain_pin_vcpu_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainReboot => 27 */
    .fn = (dispatch_fn) remoteDispatchDomainReboot,
    .args_filter = (xdrproc_t) xdr_remote_domain_reboot_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainResume => 28 */
    .fn = (dispatch_fn) remoteDispatchDomainResume,
    .args_filter = (xdrproc_t) xdr_remote_domain_resume_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainSetAutostart => 29 */
    .fn = (dispatch_fn) remoteDispatchDomainSetAutostart,
    .args_filter = (xdrproc_t) xdr_remote_domain_set_autostart_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainSetMaxMemory => 30 */
    .fn = (dispatch_fn) remoteDispatchDomainSetMaxMemory,
    .args_filter = (xdrproc_t) xdr_remote_domain_set_max_memory_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainSetMemory => 31 */
    .fn = (dispatch_fn) remoteDispatchDomainSetMemory,
    .args_filter = (xdrproc_t) xdr_remote_domain_set_memory_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainSetVcpus => 32 */
    .fn = (dispatch_fn) remoteDispatchDomainSetVcpus,
    .args_filter = (xdrproc_t) xdr_remote_domain_set_vcpus_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainShutdown => 33 */
    .fn = (dispatch_fn) remoteDispatchDomainShutdown,
    .args_filter = (xdrproc_t) xdr_remote_domain_shutdown_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainSuspend => 34 */
    .fn = (dispatch_fn) remoteDispatchDomainSuspend,
    .args_filter = (xdrproc_t) xdr_remote_domain_suspend_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainUndefine => 35 */
    .fn = (dispatch_fn) remoteDispatchDomainUndefine,
    .args_filter = (xdrproc_t) xdr_remote_domain_undefine_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* ListDefinedNetworks => 36 */
    .fn = (dispatch_fn) remoteDispatchListDefinedNetworks,
    .args_filter = (xdrproc_t) xdr_remote_list_defined_networks_args,
    .ret_filter = (xdrproc_t) xdr_remote_list_defined_networks_ret,
},
{   /* ListDomains => 37 */
    .fn = (dispatch_fn) remoteDispatchListDomains,
    .args_filter = (xdrproc_t) xdr_remote_list_domains_args,
    .ret_filter = (xdrproc_t) xdr_remote_list_domains_ret,
},
{   /* ListNetworks => 38 */
    .fn = (dispatch_fn) remoteDispatchListNetworks,
    .args_filter = (xdrproc_t) xdr_remote_list_networks_args,
    .ret_filter = (xdrproc_t) xdr_remote_list_networks_ret,
},
{   /* NetworkCreate => 39 */
    .fn = (dispatch_fn) remoteDispatchNetworkCreate,
    .args_filter = (xdrproc_t) xdr_remote_network_create_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* NetworkCreateXml => 40 */
    .fn = (dispatch_fn) remoteDispatchNetworkCreateXml,
    .args_filter = (xdrproc_t) xdr_remote_network_create_xml_args,
    .ret_filter = (xdrproc_t) xdr_remote_network_create_xml_ret,
},
{   /* NetworkDefineXml => 41 */
    .fn = (dispatch_fn) remoteDispatchNetworkDefineXml,
    .args_filter = (xdrproc_t) xdr_remote_network_define_xml_args,
    .ret_filter = (xdrproc_t) xdr_remote_network_define_xml_ret,
},
{   /* NetworkDestroy => 42 */
    .fn = (dispatch_fn) remoteDispatchNetworkDestroy,
    .args_filter = (xdrproc_t) xdr_remote_network_destroy_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* NetworkDumpXml => 43 */
    .fn = (dispatch_fn) remoteDispatchNetworkDumpXml,
    .args_filter = (xdrproc_t) xdr_remote_network_dump_xml_args,
    .ret_filter = (xdrproc_t) xdr_remote_network_dump_xml_ret,
},
{   /* NetworkGetAutostart => 44 */
    .fn = (dispatch_fn) remoteDispatchNetworkGetAutostart,
    .args_filter = (xdrproc_t) xdr_remote_network_get_autostart_args,
    .ret_filter = (xdrproc_t) xdr_remote_network_get_autostart_ret,
},
{   /* NetworkGetBridgeName => 45 */
    .fn = (dispatch_fn) remoteDispatchNetworkGetBridgeName,
    .args_filter = (xdrproc_t) xdr_remote_network_get_bridge_name_args,
    .ret_filter = (xdrproc_t) xdr_remote_network_get_bridge_name_ret,
},
{   /* NetworkLookupByName => 46 */
    .fn = (dispatch_fn) remoteDispatchNetworkLookupByName,
    .args_filter = (xdrproc_t) xdr_remote_network_lookup_by_name_args,
    .ret_filter = (xdrproc_t) xdr_remote_network_lookup_by_name_ret,
},
{   /* NetworkLookupByUuid => 47 */
    .fn = (dispatch_fn) remoteDispatchNetworkLookupByUuid,
    .args_filter = (xdrproc_t) xdr_remote_network_lookup_by_uuid_args,
    .ret_filter = (xdrproc_t) xdr_remote_network_lookup_by_uuid_ret,
},
{   /* NetworkSetAutostart => 48 */
    .fn = (dispatch_fn) remoteDispatchNetworkSetAutostart,
    .args_filter = (xdrproc_t) xdr_remote_network_set_autostart_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* NetworkUndefine => 49 */
    .fn = (dispatch_fn) remoteDispatchNetworkUndefine,
    .args_filter = (xdrproc_t) xdr_remote_network_undefine_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* NumOfDefinedNetworks => 50 */
    .fn = (dispatch_fn) remoteDispatchNumOfDefinedNetworks,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_num_of_defined_networks_ret,
},
{   /* NumOfDomains => 51 */
    .fn = (dispatch_fn) remoteDispatchNumOfDomains,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_num_of_domains_ret,
},
{   /* NumOfNetworks => 52 */
    .fn = (dispatch_fn) remoteDispatchNumOfNetworks,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_num_of_networks_ret,
},
{   /* DomainCoreDump => 53 */
    .fn = (dispatch_fn) remoteDispatchDomainCoreDump,
    .args_filter = (xdrproc_t) xdr_remote_domain_core_dump_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainRestore => 54 */
    .fn = (dispatch_fn) remoteDispatchDomainRestore,
    .args_filter = (xdrproc_t) xdr_remote_domain_restore_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainSave => 55 */
    .fn = (dispatch_fn) remoteDispatchDomainSave,
    .args_filter = (xdrproc_t) xdr_remote_domain_save_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainGetSchedulerType => 56 */
    .fn = (dispatch_fn) remoteDispatchDomainGetSchedulerType,
    .args_filter = (xdrproc_t) xdr_remote_domain_get_scheduler_type_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_get_scheduler_type_ret,
},
{   /* DomainGetSchedulerParameters => 57 */
    .fn = (dispatch_fn) remoteDispatchDomainGetSchedulerParameters,
    .args_filter = (xdrproc_t) xdr_remote_domain_get_scheduler_parameters_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_get_scheduler_parameters_ret,
},
{   /* DomainSetSchedulerParameters => 58 */
    .fn = (dispatch_fn) remoteDispatchDomainSetSchedulerParameters,
    .args_filter = (xdrproc_t) xdr_remote_domain_set_scheduler_parameters_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* GetHostname => 59 */
    .fn = (dispatch_fn) remoteDispatchGetHostname,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_get_hostname_ret,
},
{   /* SupportsFeature => 60 */
    .fn = (dispatch_fn) remoteDispatchSupportsFeature,
    .args_filter = (xdrproc_t) xdr_remote_supports_feature_args,
    .ret_filter = (xdrproc_t) xdr_remote_supports_feature_ret,
},
{   /* DomainMigratePrepare => 61 */
    .fn = (dispatch_fn) remoteDispatchDomainMigratePrepare,
    .args_filter = (xdrproc_t) xdr_remote_domain_migrate_prepare_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_migrate_prepare_ret,
},
{   /* DomainMigratePerform => 62 */
    .fn = (dispatch_fn) remoteDispatchDomainMigratePerform,
    .args_filter = (xdrproc_t) xdr_remote_domain_migrate_perform_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* DomainMigrateFinish => 63 */
    .fn = (dispatch_fn) remoteDispatchDomainMigrateFinish,
    .args_filter = (xdrproc_t) xdr_remote_domain_migrate_finish_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_migrate_finish_ret,
},
{   /* DomainBlockStats => 64 */
    .fn = (dispatch_fn) remoteDispatchDomainBlockStats,
    .args_filter = (xdrproc_t) xdr_remote_domain_block_stats_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_block_stats_ret,
},
{   /* DomainInterfaceStats => 65 */
    .fn = (dispatch_fn) remoteDispatchDomainInterfaceStats,
    .args_filter = (xdrproc_t) xdr_remote_domain_interface_stats_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_interface_stats_ret,
},
{   /* AuthList => 66 */
    .fn = (dispatch_fn) remoteDispatchAuthList,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_auth_list_ret,
},
{   /* AuthSaslInit => 67 */
    .fn = (dispatch_fn) remoteDispatchAuthSaslInit,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_auth_sasl_init_ret,
},
{   /* AuthSaslStart => 68 */
    .fn = (dispatch_fn) remoteDispatchAuthSaslStart,
    .args_filter = (xdrproc_t) xdr_remote_auth_sasl_start_args,
    .ret_filter = (xdrproc_t) xdr_remote_auth_sasl_start_ret,
},
{   /* AuthSaslStep => 69 */
    .fn = (dispatch_fn) remoteDispatchAuthSaslStep,
    .args_filter = (xdrproc_t) xdr_remote_auth_sasl_step_args,
    .ret_filter = (xdrproc_t) xdr_remote_auth_sasl_step_ret,
},
{   /* AuthPolkit => 70 */
    .fn = (dispatch_fn) remoteDispatchAuthPolkit,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_auth_polkit_ret,
},
{   /* NumOfStoragePools => 71 */
    .fn = (dispatch_fn) remoteDispatchNumOfStoragePools,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_num_of_storage_pools_ret,
},
{   /* ListStoragePools => 72 */
    .fn = (dispatch_fn) remoteDispatchListStoragePools,
    .args_filter = (xdrproc_t) xdr_remote_list_storage_pools_args,
    .ret_filter = (xdrproc_t) xdr_remote_list_storage_pools_ret,
},
{   /* NumOfDefinedStoragePools => 73 */
    .fn = (dispatch_fn) remoteDispatchNumOfDefinedStoragePools,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_num_of_defined_storage_pools_ret,
},
{   /* ListDefinedStoragePools => 74 */
    .fn = (dispatch_fn) remoteDispatchListDefinedStoragePools,
    .args_filter = (xdrproc_t) xdr_remote_list_defined_storage_pools_args,
    .ret_filter = (xdrproc_t) xdr_remote_list_defined_storage_pools_ret,
},
{   /* FindStoragePoolSources => 75 */
    .fn = (dispatch_fn) remoteDispatchFindStoragePoolSources,
    .args_filter = (xdrproc_t) xdr_remote_find_storage_pool_sources_args,
    .ret_filter = (xdrproc_t) xdr_remote_find_storage_pool_sources_ret,
},
{   /* StoragePoolCreateXml => 76 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolCreateXml,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_create_xml_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_pool_create_xml_ret,
},
{   /* StoragePoolDefineXml => 77 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolDefineXml,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_define_xml_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_pool_define_xml_ret,
},
{   /* StoragePoolCreate => 78 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolCreate,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_create_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* StoragePoolBuild => 79 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolBuild,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_build_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* StoragePoolDestroy => 80 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolDestroy,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_destroy_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* StoragePoolDelete => 81 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolDelete,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_delete_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* StoragePoolUndefine => 82 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolUndefine,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_undefine_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* StoragePoolRefresh => 83 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolRefresh,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_refresh_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* StoragePoolLookupByName => 84 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolLookupByName,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_lookup_by_name_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_pool_lookup_by_name_ret,
},
{   /* StoragePoolLookupByUuid => 85 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolLookupByUuid,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_lookup_by_uuid_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_pool_lookup_by_uuid_ret,
},
{   /* StoragePoolLookupByVolume => 86 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolLookupByVolume,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_lookup_by_volume_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_pool_lookup_by_volume_ret,
},
{   /* StoragePoolGetInfo => 87 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolGetInfo,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_get_info_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_pool_get_info_ret,
},
{   /* StoragePoolDumpXml => 88 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolDumpXml,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_dump_xml_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_pool_dump_xml_ret,
},
{   /* StoragePoolGetAutostart => 89 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolGetAutostart,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_get_autostart_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_pool_get_autostart_ret,
},
{   /* StoragePoolSetAutostart => 90 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolSetAutostart,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_set_autostart_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* StoragePoolNumOfVolumes => 91 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolNumOfVolumes,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_num_of_volumes_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_pool_num_of_volumes_ret,
},
{   /* StoragePoolListVolumes => 92 */
    .fn = (dispatch_fn) remoteDispatchStoragePoolListVolumes,
    .args_filter = (xdrproc_t) xdr_remote_storage_pool_list_volumes_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_pool_list_volumes_ret,
},
{   /* StorageVolCreateXml => 93 */
    .fn = (dispatch_fn) remoteDispatchStorageVolCreateXml,
    .args_filter = (xdrproc_t) xdr_remote_storage_vol_create_xml_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_vol_create_xml_ret,
},
{   /* StorageVolDelete => 94 */
    .fn = (dispatch_fn) remoteDispatchStorageVolDelete,
    .args_filter = (xdrproc_t) xdr_remote_storage_vol_delete_args,
    .ret_filter = (xdrproc_t) xdr_void,
},
{   /* StorageVolLookupByName => 95 */
    .fn = (dispatch_fn) remoteDispatchStorageVolLookupByName,
    .args_filter = (xdrproc_t) xdr_remote_storage_vol_lookup_by_name_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_vol_lookup_by_name_ret,
},
{   /* StorageVolLookupByKey => 96 */
    .fn = (dispatch_fn) remoteDispatchStorageVolLookupByKey,
    .args_filter = (xdrproc_t) xdr_remote_storage_vol_lookup_by_key_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_vol_lookup_by_key_ret,
},
{   /* StorageVolLookupByPath => 97 */
    .fn = (dispatch_fn) remoteDispatchStorageVolLookupByPath,
    .args_filter = (xdrproc_t) xdr_remote_storage_vol_lookup_by_path_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_vol_lookup_by_path_ret,
},
{   /* StorageVolGetInfo => 98 */
    .fn = (dispatch_fn) remoteDispatchStorageVolGetInfo,
    .args_filter = (xdrproc_t) xdr_remote_storage_vol_get_info_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_vol_get_info_ret,
},
{   /* StorageVolDumpXml => 99 */
    .fn = (dispatch_fn) remoteDispatchStorageVolDumpXml,
    .args_filter = (xdrproc_t) xdr_remote_storage_vol_dump_xml_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_vol_dump_xml_ret,
},
{   /* StorageVolGetPath => 100 */
    .fn = (dispatch_fn) remoteDispatchStorageVolGetPath,
    .args_filter = (xdrproc_t) xdr_remote_storage_vol_get_path_args,
    .ret_filter = (xdrproc_t) xdr_remote_storage_vol_get_path_ret,
},
{   /* NodeGetCellsFreeMemory => 101 */
    .fn = (dispatch_fn) remoteDispatchNodeGetCellsFreeMemory,
    .args_filter = (xdrproc_t) xdr_remote_node_get_cells_free_memory_args,
    .ret_filter = (xdrproc_t) xdr_remote_node_get_cells_free_memory_ret,
},
{   /* NodeGetFreeMemory => 102 */
    .fn = (dispatch_fn) remoteDispatchNodeGetFreeMemory,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_node_get_free_memory_ret,
},
{   /* DomainBlockPeek => 103 */
    .fn = (dispatch_fn) remoteDispatchDomainBlockPeek,
    .args_filter = (xdrproc_t) xdr_remote_domain_block_peek_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_block_peek_ret,
},
{   /* DomainMemoryPeek => 104 */
    .fn = (dispatch_fn) remoteDispatchDomainMemoryPeek,
    .args_filter = (xdrproc_t) xdr_remote_domain_memory_peek_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_memory_peek_ret,
},
{   /* DomainEventsRegister => 105 */
    .fn = (dispatch_fn) remoteDispatchDomainEventsRegister,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_domain_events_register_ret,
},
{   /* DomainEventsDeregister => 106 */
    .fn = (dispatch_fn) remoteDispatchDomainEventsDeregister,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_domain_events_deregister_ret,
},
{   /* DomainEvent => 107 */
    .fn = (dispatch_fn) remoteDispatchDomainEvent,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_domain_event_ret,
},
{   /* DomainMigratePrepare2 => 108 */
    .fn = (dispatch_fn) remoteDispatchDomainMigratePrepare2,
    .args_filter = (xdrproc_t) xdr_remote_domain_migrate_prepare2_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_migrate_prepare2_ret,
},
{   /* DomainMigrateFinish2 => 109 */
    .fn = (dispatch_fn) remoteDispatchDomainMigrateFinish2,
    .args_filter = (xdrproc_t) xdr_remote_domain_migrate_finish2_args,
    .ret_filter = (xdrproc_t) xdr_remote_domain_migrate_finish2_ret,
},
{   /* GetUri => 110 */
    .fn = (dispatch_fn) remoteDispatchGetUri,
    .args_filter = (xdrproc_t) xdr_void,
    .ret_filter = (xdrproc_t) xdr_remote_get_uri_ret,
},
{   /* NodeNumOfDevices => 111 */
    .fn = (dispatch_fn) remoteDispatchNodeNumOfDevices,
    .args_filter = (xdrproc_t) xdr_remote_node_num_of_devices_args,
    .ret_filter = (xdrproc_t) xdr_remote_node_num_of_devices_ret,
},
{   /* NodeListDevices => 112 */
    .fn = (dispatch_fn) remoteDispatchNodeListDevices,
    .args_filter = (xdrproc_t) xdr_remote_node_list_devices_args,
    .ret_filter = (xdrproc_t) xdr_remote_node_list_devices_ret,
},
{   /* NodeDeviceLookupByName => 113 */
    .fn = (dispatch_fn) remoteDispatchNodeDeviceLookupByName,
    .args_filter = (xdrproc_t) xdr_remote_node_device_lookup_by_name_args,
    .ret_filter = (xdrproc_t) xdr_remote_node_device_lookup_by_name_ret,
},
{   /* NodeDeviceDumpXml => 114 */
    .fn = (dispatch_fn) remoteDispatchNodeDeviceDumpXml,
    .args_filter = (xdrproc_t) xdr_remote_node_device_dump_xml_args,
    .ret_filter = (xdrproc_t) xdr_remote_node_device_dump_xml_ret,
},
{   /* NodeDeviceGetParent => 115 */
    .fn = (dispatch_fn) remoteDispatchNodeDeviceGetParent,
    .args_filter = (xdrproc_t) xdr_remote_node_device_get_parent_args,
    .ret_filter = (xdrproc_t) xdr_remote_node_device_get_parent_ret,
},
{   /* NodeDeviceNumOfCaps => 116 */
    .fn = (dispatch_fn) remoteDispatchNodeDeviceNumOfCaps,
    .args_filter = (xdrproc_t) xdr_remote_node_device_num_of_caps_args,
    .ret_filter = (xdrproc_t) xdr_remote_node_device_num_of_caps_ret,
},
{   /* NodeDeviceListCaps => 117 */
    .fn = (dispatch_fn) remoteDispatchNodeDeviceListCaps,
    .args_filter = (xdrproc_t) xdr_remote_node_device_list_caps_args,
    .ret_filter = (xdrproc_t) xdr_remote_node_device_list_caps_ret,
},